(in-package #:displayer)

(define-page front "/" ()
  (r-clip:with-clip-processing ((template-file "front.ctml" #.*package*) "text/html; charset=utf-8")
    (r-clip:process T
                    :message (post/get "message")
                    :error (post/get "error")
                    :videos (mapcar #'video-data (list-videos))
                    :tasks (list-tasks)
                    :video (video-running-p)
                    :runner (tasks-running-p))))
