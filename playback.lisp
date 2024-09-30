(in-package #:displayer)

(defun send-command (&rest commands)
  (telnetlib:with-telnet-session (tn (defaulted-config "localhost" :vlc-host)
                                     (defaulted-config 4212 :vlc-port))
    (telnetlib:write-ln tn (defaulted-config "vlc" :vlc-pass))
    (dolist (command commands)
      (telnetlib:write-ln tn command))))

(defun video-running-p ()
  (ignore-errors (send-command) T))

(defun playlist ()
  (vpath :data "playlist" "m3u"))

(defun make-playlist (&optional (videos (list-videos)))
  (with-open-file (stream (playlist) :direction :output :if-exists :supersede)
    (dolist (video videos videos)
      (format stream "~&~a~%" (uiop:native-namestring video)))))

(defun restart-playlist ()
  (send-command "clear" (format NIL "enqueue ~a" (namestring (playlist))) "loop on" "random on" "play"))

(defun remove-from-playlist (video)
  (send-command (format NIL "del ~a" (namestring (video-file video))))
  (make-playlist))

(defun add-to-playlist (video)
  (send-command (format NIL "enqueue ~a" (namestring (video-file video))))
  (make-playlist))
