(in-package #:displayer)

(defvar *player* NIL)

(defclass player ()
  ((process :initform NIL :accessor process)))

(defgeneric running-p (impl))
(defgeneric start (impl))
(defgeneric stop (impl))
(defgeneric play-video (video impl))
(defgeneric remove-from-playlist (video impl))
(defgeneric add-to-playlist (video impl))

(defmethod running-p ((impl (eql T)))
  (and *player* (running-p *player*)))

(defmethod start ((impl (eql T)))
  (start (or *player* (make-instance (defaulted-config 'mpv :player)))))

(defmethod stop ((impl (eql T)))
  (when *player* (stop *player*)))

(defmethod play-video (video (impl (eql T)))
  (play-video (or *player* (start impl))))

(defmethod remove-from-playlist (video (impl (eql T)))
  (remove-from-playlist video (or *player* (make-instance 'dummy))))

(defmethod add-to-playlist (video (impl (eql T)))
  (add-to-playlist video (or *player* (make-instance 'dummy))))

(defun playlist ()
  (vpath :data "playlist" "m3u"))

(defun make-playlist (&optional (videos (list-enabled-videos)))
  (with-open-file (stream (playlist) :direction :output :if-exists :supersede)
    (dolist (video videos videos)
      (format stream "~&~a~%" (pathname-utils:native-namestring video)))))

(defun restart-playlist (&optional (impl T))
  (make-playlist)
  (when (running-p impl)
    (stop impl))
  (start impl))

(defmethod remove-from-playlist :before (video (impl player))
  (make-playlist))

(defmethod add-to-playlist :before (video (impl player))
  (make-playlist))

(defmethod running-p ((player player))
  (and (process player)
       (uiop:process-alive-p (process player))))

(defmethod start :around ((player player))
  (when (or (null (process player))
            (not (uiop:process-alive-p (process player))))
    (unless (uiop:getenvp "DISPLAY")
      (setf (uiop:getenv "DISPLAY") ":0.0"))
    (setf (process player) (call-next-method)))
  (setf *player* player))

(defmethod stop ((player player))
  (when (and (process player) (uiop:process-alive-p (process player)))
    (uiop:terminate-process (process player)))
  (setf (process player) NIL)
  player)

(defclass dummy (player) ())

(defmethod remove-from-playlist (video (player dummy)))
(defmethod add-to-playlist (video (player dummy)))
(defmethod start :around ((player dummy)) player)
