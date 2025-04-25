(in-package #:displayer)

(defun output (data &optional (message "Ok."))
  (if (and (string= (post/get "browser") "true") (referer))
      (redirect (merge-url (referer) :parameters `(("message" . ,(princ-to-string message)))))
      (api-output data :message message)))

(define-api displayer/video (name) ()
  (let ((file (probe-file (video-file name))))
    (if file
        (api-output (video-data file))
        (error 'api-argument-invalid :argument 'name))))

(define-api displayer/video/file (name) ()
  (let ((file (probe-file (video-file name))))
    (cond (file
           (setf (header "Content-Disposition") (format NIL "attachment; filename=~s" (file-namestring file)))
           (serve-file file))
          (T
           (error 'api-argument-invalid :argument 'name)))))

(define-api displayer/video/thumbnail (name) ()
  (let ((file (probe-file (video-thumbnail name :create NIL))))
    (unless file
      (make-instance 'ensure-video :name name))
    (if file
        (serve-file file)
        (error 'api-argument-invalid :argument 'name))))

(define-api displayer/video/list () ()
  (api-output (mapcar #'video-data (list-videos))))

(define-api displayer/video/upload (file &optional name) ()
  (let ((name (or* name (pathname-name (second file))))
        (tmp (uiop:tmpize-pathname (make-pathname :name "video" :type "mp4" :defaults (uiop:temporary-directory)))))
    (rename-file (first file) tmp)
    (let ((task (make-instance 'add-video :input tmp :name name)))
      (output (id task) "Upload started"))))

(define-api displayer/video/download (url name) ()
  (let ((task (make-instance 'add-video :input url :name name)))
    (output (id task) "Download started")))

(define-api displayer/video/toggle (name) ()
  (let* ((enabled-p (video-enabled-p name)))
    (cond (enabled-p
           (ignore-errors (remove-from-playlist name T))
           (disable-video name))
          (T
           (enable-video name)
           (ignore-errors (add-to-playlist name T)))))
  (output NIL (if (video-enabled-p name) "Video enabled" "Video disabled")))

(define-api displayer/video/delete (name) ()
  (let ((task (make-instance 'delete-video :name name)))
    (output (id task) "Delete queued")))

(define-api displayer/video/play (name) ()
  (let ((task (make-instance 'play-video :name name)))
    (output (id task) "Video playback queued")))

(define-api displayer/playback () ()
  (api-output (mktab :status (if (running-p T) "running" "stopped"))))

(define-api displayer/playback/restart () ()
  (let ((task (make-instance 'restart-video)))
    (output (id task) "Restart queued")))

(define-api displayer/task (id) ()
  (let ((task (find-task id)))
    (if task
        (api-output (mktab :id (id task)
                           :created-at (created-at task)
                           :status (string-downcase (status task))
                           :message (message task)))
        (error 'api-argument-invalid :argument 'id))))

(define-api displayer/task/list () ()
  (api-output (loop for task in (list-tasks)
                    collect (mktab :id (id task)
                                   :created-at (created-at task)
                                   :status (string-downcase (status task))
                                   :message (message task)))))

(define-api displayer/task/clear (&optional id) ()
  (cond (id
         (let ((task (find-task id)))
           (if task
               (clear task)
               (error 'api-argument-invalid :argument 'id))
           (output T "Task cleared")))
        (T
         (clear T)
         (output T "All tasks cleared"))))

(define-api displayer/task/restart (&optional id) ()
  (cond (id
         (let ((task (find-task id)))
           (if task
               (setf (status task) :pending)
               (error 'api-argument-invalid :argument 'id))
           (output T "Task restarted")))
        (T
         (restart-task-runner)
         (output T "Task runner restarted"))))
