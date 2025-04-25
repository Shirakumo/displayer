(in-package #:displayer)

(defclass mpv (player) ())

(defun send-mpv-command (&rest command)
  (with-open-file (stream "/tmp/mpvsocket" :direction :output :if-exists :append)
    (format stream "{\"command\": [~{~s~^, ~}]}")))

(defmethod remove-from-playlist (video (player mpv))
  (let ((index (position video (list-enabled-videos))))
    (when index
      (send-mpv-command "playlist-remove" index))))

(defmethod add-to-playlist (video (player mpv))
  (send-mpv-command "loadfile" (pathname-utils:native-namestring (video-file video)) "append-play"))

(defmethod play-video (video (player mpv))
  (send-mpv-command "loadfile" (pathname-utils:native-namestring (video-file video))))

(defmethod start ((player mpv))
  (uiop:launch-program (list "mpv"
                             (format NIL "--playlist=~a" (pathname-utils:native-namestring (playlist)))
                             "--shuffle" "--loop-playlist" "--fullscreen"
                             "--input-ipc-server=/tmp/mpvsocket")
                       :output *standard-output*
                       :error-output *standard-output*))
