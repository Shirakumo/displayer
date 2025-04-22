(in-package #:displayer)

(defvar *vlc-process* NIL)

(defun send-command (&rest commands)
  (telnetlib:with-telnet-session (tn (defaulted-config "localhost" :vlc-host)
                                     (defaulted-config 4212 :vlc-port))
    (telnetlib:set-telnet-session-option tn :char-callback (constantly NIL))
    (telnetlib:write-ln tn (defaulted-config "vlc" :vlc-pass))
    (let ((read (telnetlib:read-until-2 tn '("Wrong" ">") :timeout 0.1)))
      (when (search "Wrong" read) (error "Invalid password.")))
    (dolist (command commands)
      (telnetlib:write-ln tn command)
      (telnetlib:read-available-data tn T))))

(defun video-running-p ()
  (ignore-errors (send-command) T))

(defun playlist ()
  (vpath :data "playlist" "m3u"))

(defun make-playlist (&optional (videos (list-enabled-videos)))
  (with-open-file (stream (playlist) :direction :output :if-exists :supersede)
    (dolist (video videos videos)
      (format stream "~&~a~%" (pathname-utils:native-namestring video)))))

(defun restart-playlist ()
  (make-playlist)
  (unless (video-running-p)
    (start-vlc))
  (send-command "stop" "clear" (format NIL "enqueue ~a" (namestring (playlist))) "loop on" "random on" "play"))

(defun remove-from-playlist (video)
  (send-command (format NIL "del ~a" (namestring (video-file video))))
  (make-playlist))

(defun add-to-playlist (video)
  (send-command (format NIL "enqueue ~a" (namestring (video-file video))))
  (make-playlist))

(defun play-video (video)
  (send-command "stop" "clear" (format NIL "enqueue ~a" (namestring (video-file video))) "play"
                (format NIL "enqueue ~a" (namestring (playlist))) "loop on" "random on"))

(defun start-vlc ()
  (when (or (null *vlc-process*) (uiop:process-alive-p *vlc-process*))
    (unless (uiop:getenvp "DISPLAY")
      (setf (uiop:getenv "DISPLAY") ":0.0"))
    (setf *vlc-process* (uiop:launch-program (list "vlc"
                                                   "--intf" "qt"
                                                   "--extraintf" "telnet"
                                                   "--telnet-password" (defaulted-config "vlc" :vlc-pass)
                                                   "--random" "--loop" "--fullscreen"
                                                   (pathname-utils:native-namestring (playlist)))
                                             :output *standard-output*
                                             :error-output *standard-output*)))
  *vlc-process*)

(defun stop-vlc ()
  (when (and *vlc-process* (uiop:process-alive-p *vlc-process*))
    (uiop:terminate-process *vlc-process*)
    (setf *vlc-process* NIL)))
