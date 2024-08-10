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
  (serve-file (video-file name)))

(define-api displayer/video/thumbnail (name) ()
  (serve-file (video-thumbnail name)))

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

(define-api displayer/video/delete (name) ()
  (let ((task (make-instance 'delete-video :name name)))
    (output (id task) "Delete queued")))

(define-api displayer/mpv () ()
  (api-output (mktab :status (if (mpv-running-p) "running" "stopped"))))

(define-api displayer/mpv/restart () ()
  (let ((task (make-instance 'restart-mpv)))
    (output (id task) "MPV restart queued")))

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
  (clear (if id
             (or (find-task id)
                 (error 'api-argument-invalid :argument 'id))
             T))
  (output T "Task cleared"))

(define-api displayer/task/restart () ()
  (restart-task-runner)
  (output T "Task runner restarted"))
