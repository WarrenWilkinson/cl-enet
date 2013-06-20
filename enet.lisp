;(load "quicklisp.lisp")
;(quicklisp-quickstart:install)

(load "~/quicklisp/setup.lisp")
(ql:quickload "cffi")
;(ql:quickload "lispbuilder-sdl-gfx")

(defpackage :cl-enet 
  (:use :common-lisp :cffi)
  (:export nil))

(in-package :cl-enet)

(defclass enet-event () ())
(defclass connect    (enet-event) ())
(defclass receive    (enet-event) ())
(defclass disconnect (enet-event) ())

(define-condition enet-error (error)
  ((code :initarg :enet-code :reader enet-error-code))
  (:report (lambda (c stream)
	     (format stream "libenet function returned error ~A" (enet-error-code c))))
  (:documentation "Signalled when libenet function gives us a value other than enet_ok"))

(defclass host ()
  ((ptr :initarg :ptr :initform (error "No ptr provided") :reader ptr)))


;; Two ways to look at this... I can record the NODES and their edges...
;; But I think it's better to keep them seperate. Like a graph.
;; Connections automatically registered, Disconnects auto-registered. 

;; When I receive data --- do I put that onto the host?  Or do I put it
;; onto the connection?  I think I should put it onto the connection. 
;; I might want a callback sort of situation, where when I create 
;; a connection onto my server, I register the connection with it's own
;; callback.. That callback might be 'update this' or something.
;; I might even want to name my connections... 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library enet
  (:unix (:or "libenet.so.2" "libenet.so"))
  (t (:default "libenet")))

(use-foreign-library enet)
(defctype size-t :uint)

(defcstruct address 
  "Internet Address and Port"
  (address :uint32)
  (port    :uint16))

(defcenum event-type
  (:none       0)
  (:connect    1)
  (:disconnect 2)
  (:receive    3))

(defcstruct event 
  (type       event-type)
  (peer       :pointer) ;; EnetPeer pointer
  (channel-id :uint8)
  (data       :uint32)
  (packet     :pointer))  ;; EnetPackage pointer

(defconstant +host-any+ 0)
(defconstant +port-any+ 0)
(defconstant +maximum-channel-limit+ 0)
(defconstant +maximum-bandwidth+ 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-foreign-type enet-code-type ()
;  ()
;  (:actual-type :int)
;  (:simple-parser enet-code))
;
;
;(defmethod translate-from-foreign (value (type enet-code-type))
;  (if (zerop value)
;      t
;      (error 'enet-error :enet-code value)))
;
;(define-foreign-type enet-service-result-type ()
;  ()
;  (:actual-type :int)
;  (:simple-parser enet-service-result))
;
;(defmethod translate-from-foreign (value (type enet-service-result-type))
;  (cond ((zerop value) nil)
;	((> value 0) t)
;	(t (error 'enet-error :enet-code value))))

(defcfun ("enet_initialize"   enet-initialize-cfun) :int)
(defun initialize ()
  (let ((ret (enet-initialize-cfun)))
    (if (zerop ret) t (error 'enet-error :enet-code ret))))
	
(defcfun ("enet_deinitialize" enet-deinitialize-cfun) :int)
(defun deinitialize ()
  (let ((ret (enet-deinitialize-cfun)))
    (if (zerop ret) t (error 'enet-error :enet-code ret))))

(defcfun ("enet_host_destroy" host-destroy) :void
  (host :pointer))    ;; requires an enet-host

(defcfun ("enet_packet_destroy" packed-destroy) :void
  (event :pointer))  ;; an event object to clean.

(defcfun ("enet_host_create" enet-host-create-cfun) :pointer ;; returns enet_host
  (address :pointer) ;; requires enet-address type
  (peerCount size-t)
  (channelLimit size-t)
  (incomingBandwidth :uint32)
  (outgoingBandwidth :uint32))

(defun make-host (peer-count &key
		  (address            +host-any+)
		  (port               +port-any+)
		  (channel-limit      +maximum-channel-limit+)
		  (incoming-bandwidth +maximum-bandwidth+)
		  (outgoing-bandwidth +maximum-bandwidth+))
  (with-foreign-object (ptr '(:struct address))
    (setf (foreign-slot-value ptr '(:struct address) 'address) address
	  (foreign-slot-value ptr '(:struct address) 'port) port)
    (let ((host (enet-host-create-cfun
		 ptr peer-count
		 channel-limit
		 incoming-bandwidth
		 outgoing-bandwidth)))
      (if (null-pointer-p host)
	  (error "Error occurred trying to create ENet host.")
	  (make-instance 'host :ptr host)))))

(defcfun ("enet_host_service" enet-host-service-cfun) :int
  (host :pointer)    ;; requires an enet-host
  (event :pointer)   ;; an event object to fill in
  (timeout :uint32))

(defgeneric service-event (host event-type ptr)
  (:documentation "Service a host")
  (:method ((host host) (event-type (eql :connect)) ptr)
    ;; TODO add them to peer list.
    (make-instance 'connect))
  (:method ((host host) (event-type (eql :receive)) ptr)
    ;; TODO decode the packet? Put it on the receive event
    (unwind-protect (make-instance 'receive)
      (enet-packet-destroy (foreign-slot-value ptr '(:struct enet-event) 'packet))))
  (:method ((host host) (event-type (eql :disconnect)) ptr)
    ;; TODO remove from peer list.
    (make-instance 'disconnect)))

;; TODO, does it make more sense to return the message,
;; or to queue them all up on my object in one call?
(defgeneric service (host &optional timeout)
  (:documentation "Service a host")
  (:method ((host host) &optional (timeout 0))
    (with-foreign-object (ptr '(:struct event))
      (unless (zerop (enet-host-service-cfun host ptr timeout)) ;; equal event none
	(with-foreign-slots ((type) ptr (:struct event))
	  (service-event host type ptr))))))

(defcfun ("enet_host_connect" enet-host-connect-cfun) :pointer ;; returns an enet peer
  (host    :pointer)    ;; requires an enet-host
  (address :pointer)   ;; an event object to fill in
  (channels size-t)
  (data    :uint32))

(defvar *default-port* 1234)
(defvar *default-channels* 2)
(defgeneric start-connection (host ipaddr &key port channels data)
  (:documentation "Connect to somebody")
  (:method ((host host) ipaddr &key (port *default-port*) (channels *default-channels*) (data 0))
    (assert (string-equal ipaddr "127.0.0.1"))
    (with-foreign-object (ptr '(:struct address))
      (setf (foreign-slot-value ptr '(:struct address) 'address) #x0100007F ;; #x7F000001   little endian, ugh.
	    (foreign-slot-value ptr '(:struct address) 'port) port)
      (enet-host-connect-cfun host ptr channels data))))

(defcfun "enet_peer_disconnect" :void ;; returns an enet peer
  (peer    :pointer)    ;; requires an enet-peer
  (data    :uint32))

(defgeneric start-disconnection (host peer)
  (:documentation "Queue up a disconnect packet.")
  (:method ((host host) (peer peer))
    ;; If this person is not in the peer list, then what?
    nil))

(defcfun "enet_packet_create" :pointer ;; enet-packet
  (data    :pointer)   ;; void* of data
  (length  size-t)
  (flags   :uint32))

(defcenum enet-packet-flag
  (:reliable     #b00000001)
  (:unsequenced  #b00000010)
  (:no-allocate  #b00000100)  ;; Not used by me
  (:can-fragment #b00001000)
  (:sent         #b10000000)) ;; Not used by me 

(defcfun "enet_peer_send" enet-code 
  (peer    :pointer)   ;; enet-peer 
  (channel :uint8)
  (packet  :pointer))  ;; enet-packet

(defvar *default-channel* 0)
(defun enet-send (peer data &key (channel *default-channel*) reliable unsequenced can-fragment)
  (with-foreign-pointer (string (length data) size)
    ;; we don't copy it in yet, so in effect, we send garbage
    (let ((packet (enet-packet-create string size
				      (boole boole-ior
					     (boole boole-ior
						    (if reliable
							(foreign-enum-value 'enet-packet-flag :reliable)
							0)
						    (if unsequenced 
							(foreign-enum-value 'enet-packet-flag :unsequenced)
							0))
					     (if can-fragment 
						 (foreign-enum-value 'enet-packet-flag :can-fragment)
						 0)))))
      (enet-peer-send peer channel packet))))
    


;; How do I want to handle the results of this?  I return t/nil if there is or isn't something to handle.
;; In C you'd want to put that into a list. I could macro it, but that's lousy.  I could translate the event
;; to lisp and return that... that's probably the best option. There are only three event types.
;; I should collect the 'peer' pointers internally, so I can just return the pointer to that. You can ask
;; C for details.
;; THe packet should be translated, as should the flags.
;; Ignore client data. Instead they can assoc based on the client in a hashtable or whatever.

(enet-initialize)
(defvar *host* (enet-host-create 32 :port 1234))
(defvar *client* (enet-host-create 32))
(list *host* *client*)

;; Great, this stuff works now. I can connect and handle my connection event.
(defvar *client-to-host* (enet-host-connect *client* "127.0.0.1" :port 1234 :channels 2))
(list *client-to-host*)
(enet-host-service *client*)
(enet-host-service *host*)

;; Next up, I should make them send data...

(enet-send *client-to-host* (sb-ext:string-to-octets "Hello World!") :channel 0)
(enet-host-service *client*)
(enet-host-service *host*)


;; and then recieve it... 


;; And now send the response....


;; and the client receives it...


;; Then disconnect
(enet-peer-disconnect *client-to-host* 0)


;; Then shutdown
(enet-host-destroy *client*)
(enet-host-destroy *host*)
(enet-deinitialize)


;; Once I reach this point, it's time to clean up and define higher level structure.
;; Events should be passed upwards, but peers should be recorded with the host...
;; (prevent duplicate connections, etc). Ability to query connections.
;; Data attached to packet.  Lets do an ECHO server, than a PING PONG server
;; then a chat server.  Then this library is probably good.

