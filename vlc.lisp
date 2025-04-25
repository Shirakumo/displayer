(in-package #:displayer)

(defclass vlc (player) ())

(defun send-vlc-command (&rest commands)
  (telnetlib:with-telnet-session (tn (defaulted-config "localhost" :vlc-host)
                                     (defaulted-config 4212 :vlc-port))
    (telnetlib:set-telnet-session-option tn :char-callback (constantly NIL))
    (telnetlib:write-ln tn (defaulted-config "vlc" :vlc-pass))
    (let ((read (telnetlib:read-until-2 tn '("Wrong" ">") :timeout 0.1)))
      (when (search "Wrong" read) (error "Invalid password.")))
    (dolist (command commands)
      (telnetlib:write-ln tn command)
      (telnetlib:read-available-data tn T))))

(defmethod remove-from-playlist (video (player vlc))
  (send-vlc-command (format NIL "del ~a" (namestring (video-file video)))))

(defmethod add-to-playlist (video (player vlc))
  (send-vlc-command (format NIL "enqueue ~a" (namestring (video-file video)))))

(defmethod play-video (video (player vlc))
  (send-vlc-command "stop" "clear" (format NIL "enqueue ~a" (namestring (video-file video))) "play"
                (format NIL "enqueue ~a" (namestring (playlist))) "loop on" "random on"))

(defmethod start ((player vlc))
  (uiop:launch-program (list "vlc"
                             "--intf" "qt"
                             "--extraintf" "telnet"
                             "--telnet-password" (defaulted-config "vlc" :vlc-pass)
                             "--random" "--loop" "--fullscreen"
                             (pathname-utils:native-namestring (playlist)))
                       :output *standard-output*
                       :error-output *standard-output*))
