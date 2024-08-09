(in-package #:displayer)

(defvar *mpv-process* NIL)
(defvar *task-runner* NIL)
(defvar *task-condition* (bt:make-condition-variable :name "task runner condition"))
(defvar *task-lock* (bt:make-lock "task runner lock"))
(defvar *tasks* (make-hash-table :test 'equal))

(defun restart-mpv-process ()
  (when *mpv-process*
    (when (uiop:process-alive-p *mpv-process*)
      (uiop:terminate-process *mpv-process*))
    (setf *mpv-process* NIL))
  (setf *mpv-process* (uiop:launch-program (list "mpv" "--shuffle" "--loop-playlist=inf" (uiop:native-namestring (playlist))))))

(defun mpv-running-p ()
  (and *mpv-process* (uiop:process-alive-p *mpv-process*)))

(define-trigger radiance:startup-done ()
  (restart-mpv-process)
  (restart-task-runner))

(defun restart-task-runner ()
  (when *task-runner*
    (when (bt:thread-alive-p *task-runner*)
      (make-instance 'stop-task-runner)
      (loop repeat 10
            do (unless (bt:thread-alive-p *task-runner*)
                 (return))
               (sleep 0.01)
            finally (bt:destroy-thread *task-runner*)))
    (setf *task-runner* NIL))
  (setf *task-runner* (bt:make-thread #'run-tasks :name "displayer task runner")))

(defun run-tasks ()
  (with-simple-restart (stop-task-runner "Stop the task runner")
    (loop (loop for task in (bt:with-lock-held (*task-lock*)
                              (loop for task being the hash-values of *tasks*
                                    when (eql :pending (status task))
                                    collect task))
                do (when (eql :pending task)
                     (with-simple-restart (abort "Abort the task")
                       (handler-bind ((error #'abort))
                         (execute task)))))
          (bt:with-lock-held (*task-lock*)
            (bt:condition-wait *task-condition* *task-lock* :timeout 5)))))

(defmethod find-task ((id string))
  (bt:with-lock-held (*task-lock*)
    (gethash id *tasks*)))

(defun list-tasks ()
  (bt:with-lock-held (*task-lock*)
    (alexandria:hash-table-values *tasks*)))

(defmethod clear ((task task))
  (bt:with-lock-held (*task-lock*)
    (remhash (id task) *tasks*)))

(defmethod clear ((all (eql T)))
  (bt:with-lock-held (*task-lock*)
    (clrhash *tasks*)))

(defclass task ()
  ((id :initarg :id :initform (make-random-string) :accessor id)
   (status :initform :pending :accessor status)
   (message :initform NIL :accessor message)))

(defmethod initialize-instance :after ((task task) &key)
  (bt:with-lock-held (*task-lock*)
    (setf (gethash (id task) *tasks*) task))
  (bt:condition-notify *task-condition*))

(defgeneric execute (task))

(defmethod execute :around ((task task))
  (setf (status task) :running)
  (handler-bind ((error (lambda (e)
                          (setf (message task) (princ-to-string e))
                          (setf (status task) :failed))))
    (prog1 (call-next-method)
      (setf (status task) :finished))))

(defclass add-video (task)
  ((input :initarg :input :accessor input)
   (name :initarg :name :accessor name)))

(defmethod execute ((task add-video))
  (copy-video (input task) (name task))
  (make-playlist)
  (restart-mpv-process))

(defclass delete-video (task)
  ((name :initarg :name :accessor name)))

(defmethod execute ((task delete-video))
  (uiop:delete-file-if-exists (video-thumbnail (video-file (name task))))
  (uiop:delete-file-if-exists (video-file (name task)))
  (make-playlist)
  (restart-mpv-process))

(defclass restart-mpv (task)
  ())

(defmethod execute ((task restart-mpv))
  (restart-mpv-process))

(defclass stop-task-runner (task)
  ())

(defmethod execute ((task stop-task-runner))
  (invoke-restart 'stop-task-runner))
