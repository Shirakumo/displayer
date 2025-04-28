(in-package #:displayer)

(defvar *task-runner* NIL)
(defvar *task-condition* (bt:make-condition-variable :name "task runner condition"))
(defvar *task-lock* (bt:make-lock "task runner lock"))
(defvar *tasks* (make-hash-table :test 'equal))

(defun tasks-running-p ()
  (and *task-runner* (bt:thread-alive-p *task-runner*)))

(defun restart-task-runner ()
  (when *task-runner*
    (when (bt:thread-alive-p *task-runner*)
      (let ((stop (make-instance 'stop-task-runner)))
        (unwind-protect
             (loop repeat 10
                   do (unless (bt:thread-alive-p *task-runner*)
                        (return))
                      (sleep 0.01)
                   finally (bt:destroy-thread *task-runner*))
          (clear stop))))
    (setf *task-runner* NIL))
  (setf *task-runner* (bt:make-thread #'run-tasks :name "displayer task runner")))

(defun run-tasks ()
  (with-simple-restart (stop-task-runner "Stop the task runner")
    (loop (loop for task in (bt:with-lock-held (*task-lock*)
                              (loop for task being the hash-values of *tasks*
                                    when (eql :pending (status task))
                                    collect task))
                do (with-simple-restart (abort "Abort the task")
                     (handler-bind ((error (lambda (e)
                                             (l:debug :displayer e)
                                             (l:error :displayer "Task ~a failed: ~a" task e)
                                             (abort e))))
                       (execute task))))
          (bt:with-lock-held (*task-lock*)
            (bt:condition-wait *task-condition* *task-lock* :timeout 5)))))

(defmethod find-task ((id string))
  (bt:with-lock-held (*task-lock*)
    (gethash id *tasks*)))

(defun list-tasks ()
  (sort (bt:with-lock-held (*task-lock*)
          (alexandria:hash-table-values *tasks*))
        #'> :key #'created-at))

(defmethod clear ((all (eql T)))
  (bt:with-lock-held (*task-lock*)
    (clrhash *tasks*)))

(defclass task ()
  ((id :initarg :id :initform (make-random-string) :accessor id)
   (created-at :initarg :created-at :initform (get-universal-time) :accessor created-at)
   (status :initform :pending :accessor status)
   (message :initform NIL :accessor message)))

(defmethod initialize-instance :after ((task task) &key)
  (bt:with-lock-held (*task-lock*)
    (setf (gethash (id task) *tasks*) task))
  (bt:condition-notify *task-condition*))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type T)
    (format stream "~a ~a" (id task) (status task))))

(defgeneric execute (task))

(defmethod execute :around ((task task))
  (l:info :displayer "Running ~a" task)
  (setf (status task) :running)
  (handler-bind ((error (lambda (e)
                          (l:info :displayer e)
                          (l:error :displayer "Task ~a failed: ~a" task e)
                          (setf (message task) (princ-to-string e))
                          (setf (status task) :failed))))
    (prog1 (call-next-method)
      (setf (status task) :finished))))

(defmethod descriptor ((task task)) "")

(defmethod clear ((task task))
  (bt:with-lock-held (*task-lock*)
    (remhash (id task) *tasks*)))

(defclass ensure-video (task)
  ((name :initarg :name :accessor name :reader descriptor)))

(defmethod execute ((task ensure-video))
  (video-thumbnail (name task))
  (video-length (name task)))

(defclass add-video (task)
  ((input :initarg :input :accessor input)
   (name :initarg :name :accessor name :reader descriptor)))

(defmethod execute ((task add-video))
  (copy-video (input task) (name task))
  (ignore-errors (add-to-playlist (name task) T)))

(defclass delete-video (task)
  ((name :initarg :name :accessor name :reader descriptor)))

(defmethod execute ((task delete-video))
  (ignore-errors (remove-from-playlist (name task) T))
  (delete-video (name task)))

(defclass restart-video (task)
  ())

(defmethod execute ((task restart-video))
  (restart-playlist))

(defclass play-video (task)
  ((name :initarg :name :accessor name :reader descriptor)))

(defmethod execute ((task play-video))
  (play-video (name task) T))

(defclass stop-task-runner (task)
  ())

(defmethod execute ((task stop-task-runner))
  (invoke-restart 'stop-task-runner))
