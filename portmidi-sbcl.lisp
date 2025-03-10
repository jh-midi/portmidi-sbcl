
(defpackage :portmidi
  (:use  :cl-user :sb-alien :common-lisp)
  (:nicknames :pm :pt)
  (:shadow :initialize :terminate :time :start :stop :sleep :abort 
           :close :read :write :poll)

 (:export :Initialize :Terminate 
           :Has-Host-Error :Get-Error-Text :Get-Host-Error-Text 
           :Count-Devices
           :Get-Device-Info :get-device-id
           :Message :Message-status :Message-data1 :Message-data2
           :Event-message :Event-timestamp 
           ;; event buffers added to api
           :Event-Buffer-New :Event-Buffer-Free :Event-Buffer-Elt
           :Event-Buffer-Map
	   :Create-Virtual-Input :Create-Virtual-Output :Delete-Virtual-Device
           :Open-Input :Open-Output :Set-Filter :Set-Channel-Mask
           :Abort :Close :Read :Write :Poll :Write-Short :Write-Sysex
           ;; filtering constants
	   :filtering-alist
           ;; porttime.
           :Start :Stop :Started :Time
           ;; initialization insurers added to api
          :portmidi :*portmidi* ))

(in-package :portmidi)

(defvar *libportmidi*
  (let ((type #+(or darwin macos macosx) "dylib"
              #+(or linux linux-target (and unix pc386) freebsd) "so"
              #+(or win32 microsoft-32 cygwin) "dll")
	(name #+(or darwin macos macosx  linux linux-target (and unix pc386) freebsd) "libportmidi"
              #+(or win32 microsoft-32 cygwin) "portmidi")
        (paths (list "/usr/lib/" "/usr/local/lib/" *load-pathname*)))
    (loop for d in paths
       for p = (make-pathname :name name :type type 
                              :defaults d)
       when (probe-file p) do (return p)
       finally  
         (error "Library \"~A.~A\" not found. Fix *lib portmidi*."
                name type))))

(sb-alien:load-shared-object *libportmidi*)

(defparameter filtering-alist
  ;;"Filter bit-mask definitions"
  '((:active 16384 ) 
    (:sysex 1) 
    (:clock 256)
    (:play 7168)
    (:tick  512)
    (:fd 8192)
    (:undefined 8192)
    (:reset 32768)
    (:realtime 65281)
    (:note 50331648)
    (:channel-aftertouch 536870912)
    (:poly-aftertouch 67108864) 
    (:aftertouch 603979776) 
    (:program 268435456) 
    (:control 134217728) 
    (:pitchbend 1073741824) 
    (:mtc 2) 
    (:song-position 4) 
    (:song-select 8)
    (:tune 64) 
    (:systemcommon 78)))

;; (defparameter on (message #b10010000 60 64))
;; (defparameter off (message #b10000000 60 64))
(declaim (inline message))
(defun message (status &optional (data1 0) (data2 0))
  "Encode a short MIDI message into four bytes."
  (declare (type (unsigned-byte 8) status data1 data2))
  (logior (ash data2 16) (ash data1 8) status))


(declaim (inline message-type))
(defun message-type (msg)
  "8 #b1000 note off 9 #b1001 note on etc..."
  (declare (type integer msg))
   (ldb (byte 4 4) msg))

(declaim (inline message-status))
(defun message-status (msg)
  (declare (type integer msg))
  (logand msg #xFF))

(declaim (inline message-data1))
(defun message-data1 (msg)
  (declare (type integer msg))
  (ldb (byte 8 8) msg))

(declaim (inline message-data2))
(defun message-data2 (msg)
  (declare (type integer msg))
  (ldb (byte 8 16) msg))

(declaim (inline decode-message))
(defun decode-message (msg)
  "Decode a MIDI message encoded into four bytes."
  (declare (type integer msg))
  (let ((ash-8 (ldb (byte 16 8) msg)))
    (values (ldb (byte 8 0) msg)       ; status
            (ldb (byte 8 0) ash-8)     ; data1
            (ldb (byte 8 8) ash-8))))  ; data2

(declaim (inline decode-channel-message))
(defun decode-channel-message (msg)
  (declare (type integer msg))
  (let* ((ash-4 (ldb (byte 20 4) msg))
         (ash-8 (ldb (byte 16 4) ash-4)))
    (values (ldb (byte 4 0) ash-4)     ; type
            (ldb (byte 4 0) msg)       ; channel
            (ldb (byte 8 0) ash-8)     ; data1
            (ldb (byte 8 8) ash-8))))  ; data2

(declaim (inline before))
(defun before (t1 t2)
  (< t1 t2))

(declaim (inline channel))
(defun channel (chan)
  "for channel-mask"
  (declare (type (unsigned-byte 4) chan))
  (ash 1 chan))

;; sbcl standalone FFI
(define-alien-type nil
    (struct  pm-device-info 
            (struct-version int) 
		 (interf c-string) 
		 (name c-string) 
		 (input int) 
		 (output int) 
		 (opened int)))

(define-alien-type pm-error int)
(define-alien-type pm-message long)
(define-alien-type pm-timestamp long)

(define-alien-type nil
    (struct pm-event 
	    (message pm-message) 
	    (timestamp pm-timestamp)))


(define-alien-type pm-error int)
(define-alien-type pm-message long)
(define-alien-type pm-timestamp long)

(define-alien-routine "Pt_Started" int )
(defun started () (pt-started))
(define-alien-routine "Pt_Time" long)
(defun time () (pt-time))
(define-alien-routine "Pt_Start" pm-error (resolution int) (callback (* t)) (userdata (* t)))
(define-alien-routine "Pt_Stop" pm-error)
(defun stop () (pt-stop))
(define-alien-routine "Pt_Sleep" void (duration long))
(defun sleep (duration) (pt-sleep duration))


(define-alien-routine "Pm_CountDevices" int)
(defun count-devices () (pm-countDevices))
(define-alien-routine "Pm_GetErrorText" c-string  (errnum pm-error))
(defun get-error-text (err-num) (pm-getErrorText err-num))
(define-alien-routine "Pm_GetHostErrorText" void (msg c-string)  (len unsigned)) 
(define-alien-routine "Pm_HasHostError" int (stream (* t)))
(defun has-host-error (stm) (pm-hasHostError stm))
  
(define-alien-routine "Pm_Initialize" pm-error)
(defun initialize () (pm-initialize))

(define-alien-routine "Pm_Terminate"  pm-error)
(defun terminate () (pm-terminate))


;;(pm-countdevices)

(define-alien-routine "Pm_GetDeviceInfo" (* (struct pm-device-info)) (device-id int))
;;(setq jo (pm-getdeviceinfo 1))
;;(slot jo 'name)


(define-alien-routine "Pm_Close" pm-error (stream (* t)))
(defun close (stm) (pm-close stm))
(define-alien-routine "Pm_Abort" pm-error (stream (* t)))
(defun abort (stm) (pm-abort stm))
(define-alien-routine "Pm_SetFilter" pm-error (stream (* t) ) (filters long))
 ;; TODO
(define-alien-routine "Pm_SetChannelMask" pm-error (stream (* t)) (mask int))
(defun set-channel-mask  (stm chan) (pm-setchannelmask stm  (channel chan))) 
(define-alien-routine "Pm_Synchronize" pm-error (stream (* t)))
(defun synchronize (stm) (pm-synchronize stm))


(define-alien-routine  "Pm_CreateVirtualInput" pm-error
  (name c-string) (interf c-string) (deviceinfo (* t)))

(define-alien-routine  "Pm_CreateVirtualOutput" pm-error
  (name c-string) (interf c-string) (deviceinfo (* t)))

(define-alien-routine  "Pm_DeleteVirtualDevice" pm-error (device-id int))

(define-alien-routine "Pm_OpenInput" pm-error
  (stream   (* t))
  (input-device int)
  (input-driver-info (* t))
  (buffer-size long)
  (time-proc (* t))
  (time-info  (* t) )) 


(define-alien-routine "Pm_OpenOutput" pm-error
  (stream  (* t))
  (output-device-id int)
  (output-driver-info (* t))
  (buffer-size long)
  (time-proc (* t))
  (time-info  (* t) )
  (latency long)) 

;;; reading writing
(define-alien-routine "Pm_Poll" pm-error (stream (* t)))
(define-alien-routine "Pm_Read" int (stream (* t)) (buffer (* (struct pm-event))) (length long))
(define-alien-routine "Pm_Write" pm-error (stream (* t)) (buffer (*(struct  pm-event))) (length long))
(define-alien-routine "Pm_WriteShort" pm-error (stream (* t)) (when! pm-timestamp) (msg pm-message))
(define-alien-routine "Pm_WriteSysEx" pm-error (stream (* t)) (when! pm-timestamp) (msg pm-message))


(defun start (&optional (resolution 1))
  (with-alien  ((callback (* t))  (userdata (* t)))
    (pt-start resolution callback userdata)))


(defun create-virtual-input (name &optional (interf "ALSA"))
  (when (zerop (pt-started)) (start))
    (with-alien ((device-info (* t)))
      (let ((dev-id (pm-CreateVirtualInput name interf  device-info)))
	(if (not (minusp dev-id) ) ;; no error
	 dev-id ;; alias for the new id
	 (error  (pm-getErrorText  dev-id))))))

(defun create-virtual-output (name &optional (interf "ALSA"))
  (when (zerop (pt-started)) (start))
    (with-alien ((device-info (* t)))
      (let ((dev-id (pm-CreateVirtualOutput name interf  device-info)))
	(if (not (minusp dev-id) ) ;; no error
	 dev-id ;; alias for the new id
	 (error  (pm-getErrorText  dev-id))))))


(defun delete-virtual-device (device-id)
   (when (zerop (pt-started)) (start))
   (let ((err (pm-deleteVirtualDevice device-id)))
     (if (not (minusp err) ) ;; no error
	 err 
	 (error  (pm-getErrorText  err)))))

;; (defparameter inid 1)
;; (defparameter midin (open-input inid 256))
(defun open-input  (device-id bufsiz)
  (when (zerop (pt-started)) (start))
  (let ((stream (make-alien (* (* t))))) ;; initialize one pointer to pointer -- malloc
    (with-alien ((input-driver-info (* t))
		 (time-proc (* t))
	         (time-info (* t)))
      (let ((err (Pm-OpenInput stream device-id input-driver-info bufsiz time-proc time-info)))
        (if (zerop err)
   	    (deref stream)
            (error (pm-getErrorText err)))))))

;; (defparameter midout (open-output 5 100 1000))
(defun open-output  (device-id bufsiz latency)
  (when (zerop (pt-started)) (start))
  (let ((stream (make-alien (* (* t))))) ;; initialize one pointer to pointer -- malloc
    (with-alien ((output-driver-info (* t))
		 (time-proc (* t))
		 (time-info (* t)))
      (let ((err (Pm-OpenOutput  stream
				 device-id
				 output-driver-info
				 bufsiz time-proc
				 time-info
				 latency)))
        (if (zerop err)
            (deref stream)
            (error (pm-getErrorText err))))
      )))

(defun event-message (event)
  (slot event 'message))

(defun event-timestamp (event)
   (slot event 'timestamp))

;; (defparameter buff4 (event-buffer-new 4))
(defun event-buffer-new (len)
  (make-alien  (struct pm-event) len))

(defun event-buffer-free (buf)
  (free-alien buf))

(defun event-buffer-elt (buf i)
  (deref buf i ))


(defun event-buffer-set (buffer index new-timestamp new-message)
  (setf (slot (deref buffer index) 'timestamp) new-timestamp)
  (setf (slot (deref buffer index) 'message) new-message)
 (values))

;;  (event-buffer-map (lambda (a b) b (terpri) (print-midi a)) buff4 num)
(defun event-buffer-map (fn buf end)
  (loop for i below end
     for e = (event-buffer-elt buf i)
     do (funcall fn (slot e 'message) (slot e 'timestamp )))
 (values))

;; (defparameter num (read midin buff4 4))
(defun read (pms *evbuf len) 
  (let ((res (pm-read pms *evbuf len)))
    (if (minusp res)
        (error (pm-getErrorText  res))
        res)))

;;  (poll midin)
(defun poll (pms)
  (let ((res (pm-poll pms)))
    (cond ((= res 0) nil)
          ((= res 1) t)
          (t (error (pm-getErrorText  res))))))


(defun write (pms *evbuf len) 
  (let ((res (pm-write pms *evbuf len)))
    (if (minusp res)
        (error (pm-getErrorText  res))
        res)))

(defun write-short (pms when msg) 
  (let ((res (pm-writeShort pms when msg)))
    (if (minusp res)
        (error (pm-getErrorText  res))
        res)))

(defun write-sysex (pms when sysex-msg)
   (let ((res (pm-writeSysex pms when sysex-msg)))
    (if (minusp res)
        (error (pm-getErrorText  res))
        res)))
  
(defun get-device-info (&optional id)
  "return a list of property list"
  (flet ((getone (id)
           (let ((d (pm-getdeviceinfo id)))
	     (if (null-alien d) ;null pointer when id is deleted or out of range
		 (list :id id :name "none" :type nil :open nil)  
		 (list :id id
                       :name  (slot d 'name )
                       :type (if (zerop (slot d 'input )) ':output ':input )
                       :open (slot d 'opened ))))))
    (if id (getone id)
        (loop for i below (pm-CountDevices)
              collect (getone i)))))

(defun get-host-error-text ()
  (let ((host-error-text (make-string 256 :initial-element #\*)))
    (pm-getHostErrorText host-error-text 256)
  host-error-text))

;; (pm:get-device-id "Microsoft GS Wavetable Synth") => 5
;; (pm:get-device-id "loopMIDI Port" :input) => 1
(defun get-device-id (name &optional (direction :output))
  "direction is :output or :input"
  (let ((found nil))
    (do* ((id (1- (count-devices)) (1- id))
	  (info (get-device-info id) (get-device-info id)))
	  ((or found (minusp id)) found) 
     	  (when (and ( string= (getf info  :name) name)
		     (eq (getf info :type) direction))
	    (setf found id)))))

;; (set-filter midin :pitchbend :realtime :control)
(defun set-filter (stm &rest filter)
  (let* ((list-all (loop for f in filter collect (cadr (assoc f filtering-alist))))
	 (main-filter (apply #'logior list-all)))
    (pm-setfilter stm main-filter)
       ))






(defun print-midi (m &optional (s t))
  (format s "#<message :op ~2,'0x :ch ~2,'0d :data1 ~3,'0d :data2 ~3,'0d>"
	  (ash (logand (message-status m ) #xf0) -4)
          (logand   (message-status m ) #x0f)
          (message-data1 m)
          (message-data2 m)))




;; make sure lib is initialized
(pm-initialize)
