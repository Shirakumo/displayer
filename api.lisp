(in-package #:displayer)

(define-api displayer/video (name) ()
  (let ((file (probe-file (video-file name))))
    (if file
        (api-output (mktab :name name
                           :file (uri-to-url "/api/displayer/video/file" :representation :external)
                           :thumbnail (uri-to-url "/api/displayer/video/thumbnail" :representation :external)
                           :size (file-size file))))))

(define-api displayer/video/file (name) ()
  (serve-file (video-file name)))

(define-api displayer/video/thumbnail (name) ()
  (serve-file (video-thumbnail name)))

(define-api displayer/video/list () ()
  (api-output (mapcar #'video-name (list-videos))))

(define-api displayer/video/upload (file name) ()
  (let ((task (make-instance 'add-video :input file :name name)))
    (api-output (id task))))

(define-api displayer/video/download (url name) ()
  (let ((task (make-instance 'add-video :input url :name name)))
    (api-output (id task))))

(define-api displayer/video/delete (name) ()
  (let ((task (make-instance 'delete-video :name name)))
    (api-output (id task))))

(define-api displayer/mpv () ()
  (api-output (mktab :status (if (mpv-running-p) "running" "stopped"))))

(define-api displayer/mpv/restart () ()
  (let ((task (make-instance 'restart-mpv)))
    (api-output (id task))))

(define-api displayer/task (id) ()
  (let ((task (find-task id)))
    (if task
        (api-output (mktab :status (string-downcase (status task))
                           :message (message task)))
        (error 'api-argument-invalid :argument 'id))))

(define-api displayer/task/list () ()
  (api-output (list-tasks)))

(define-api displayer/task/clear (&optional id) ()
  (clear (if id
             (or (find-task id)
                 (error 'api-argument-invalid :argument 'id))
             T))
  (api-output T))
