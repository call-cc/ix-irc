(define-module (ix irc)
  #:use-module (oop goops)
  #:use-module (8sync)
  #:use-module (8sync actors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (<irc>
            *irc-eol*
            irc-username
            irc-nickname
            irc-ident
            irc-server
            irc-channels
            irc-port
            irc-socket

            get-nick
            is-target-channel?
            is-target-user?

            irc-privmsg
            irc-motd-end
            irc-nick-taken
            irc-nick
            irc-msg))

(define *irc-eol* "\r\n")

(define* (irc-socket-setup hostname #:optional (inet-port 6667))
  (let* ((s (socket PF_INET SOCK_STREAM 0))
         (flags (fcntl s F_GETFL))
         (ip-address (inet-ntop AF_INET (car (hostent:addr-list (gethost hostname))))))
    (fcntl s F_SETFL (logior O_NONBLOCK flags))
    (connect s AF_INET
             (inet-pton AF_INET ip-address)
             inet-port)
    s))

(define-class <irc> (<actor>)
  (username #:init-keyword #:username
            #:getter irc-username)
  (nick #:init-keyword #:nick
        #:getter irc-nickname)
  (realname #:init-keyword #:realname
            #:getter irc-realname)
  (ident #:init-keyword #:ident
         #:getter irc-ident)
  (nick-password #:init-keyword #:nick-password
                 #:getter irc-nick-password)
  (server #:init-keyword #:server
          #:getter irc-server)
  (channels #:init-keyword #:channels
            #:getter irc-channels)
  (port #:init-keyword #:port
        #:init-value 6667
        #:getter irc-port)
  (socket #:accessor irc-socket)
  (actions #:allocation #:each-subclass
           #:init-thunk (build-actions
                         (*init* irc-init)
                         (*cleanup* irc-cleanup)
                         (handle-line handle-line)
                         (irc-ping irc-ping)
                         (irc-pong irc-pong)
                         (irc-join irc-join)
                         (irc-part irc-part)
                         (irc-privmsg irc-privmsg)
                         (irc-motd irc-motd)
                         (irc-motd-start irc-motd-start)
                         (irc-motd-end irc-motd-end)
                         (irc-nick-taken irc-nick-taken)
                         (irc-nick irc-nick)
                         (main-loop irc-main-loop))))

(define (irc-init irc message)
  (define socket
    (irc-socket-setup (irc-server irc)
                      (irc-port irc)))
  (set! (irc-socket irc) socket)
  (format socket "USER ~a ~a ~a :~a~a"
          (irc-ident irc)
          "*" "*"
          (irc-realname irc) *irc-eol*)
  (<- (actor-id irc) 'irc-nick
      (irc-username irc))
  (<- (actor-id irc) 'main-loop))

(define (irc-cleanup irc message)
  (close (irc-socket irc)))

(define (irc-main-loop irc message)
  (define socket (irc-socket irc))
  (define line (string-trim-right (read-line socket) #\return))
  (dispatch-raw-line irc line)
  (cond
   ;; The port's been closed for some reason, so stop looping
   ((port-closed? socket)
    'done)
   ;; We've reached the EOF object, which means we should close
   ;; the port ourselves and stop looping
   ((eof-object? (peek-char socket))
    (close socket)
    'done)
   (else
    (<- (actor-id irc) 'main-loop))))

(define re-message (make-regexp "^((:(\\S+) (\\w+|\\d{3}))|([a-z]+))( (:?.+))*" regexp/icase))

(define (is-prefix-msg? line)
  (and (> (string-length line) 0)
       (char=? (string-ref line 0)
               #\:)))

(define (remove-prefix-colon str)
  (if (is-prefix-msg? str)
      (substring str 1)
      str))

(define (fix-colon-param params)
  (if (null? params)
      '()
      (if (is-prefix-msg? (car params))
          (list (remove-prefix-colon (string-join params)))
          (cons (car params)
                (fix-colon-param (cdr params))))))

(define (get-params params)
  (and params
       (delete "" (fix-colon-param (string-split params #\space)))))

(define (parse-line line)
  (let* ((m (regexp-exec re-message line))
         (p-prefix (match:substring m 3))
         (p-cmd (match:substring m 4))
         (p-extra (match:substring m 6))
         (p-params (get-params p-extra))
         (cmd (match:substring m 5))
         (extra (match:substring m 7))
         (params (get-params extra)))
    (if (is-prefix-msg? line)
        (values p-prefix p-cmd p-params)
        (values #f cmd params))))

(define (get-nick user)
  (car (string-split user #\!)))

(define (is-target-channel? target)
  (and (member (string-ref target 0)
               '(#\& #\# #\+ #\!))
       #t))

(define (is-target-user? target)
  (not (is-target-channel? target)))

(define *irc-functions* '(("372" . irc-motd)
                          ("375" . irc-motd-start)
                          ("376" . irc-motd-end)
                          ("433" . irc-nick-taken)
                          ("PING" . irc-pong)
                          ("JOIN" . irc-join)
                          ("PART" . irc-part)
                          ("PRIVMSG" . irc-privmsg)))

(define-method (dispatch-raw-line (irc <irc>) raw-line)
  "Dispatch a raw line of input"
  (receive (line-prefix line-command line-params)
      (parse-line raw-line)
    (let ((func (assoc line-command *irc-functions* string-ci=?)))
     (if func
         (<- (actor-id irc) (cdr func) (list line-prefix line-params))
         (handle-line irc raw-line)))))

(define-method (handle-line (irc <irc>) line)
  (display line)
  (newline))

(define-method (irc-pong (irc <irc>) message params)
  (display (string-append "PONG" *irc-eol*)
           (irc-socket irc))
  (<- (actor-id irc) 'irc-ping params))

(define-method (irc-ping (irc <irc>) message params))

(define-method (irc-join (irc <irc>) message params))

(define-method (irc-part (irc <irc>) message params))

(define-method (irc-privmsg (irc <irc>) message params))

(define-method (irc-motd (irc <irc>) message params))

(define-method (irc-motd-start (irc <irc>) message params))

(define-method (irc-motd-end (irc <irc>) message params)
  (for-each
   (lambda (channel)
     (let ((socket (irc-socket irc)))
       (if (pair? channel)
           (format socket "JOIN ~a ~a~a" (car channel) (cdr channel) *irc-eol*)
           (format socket "JOIN ~a~a" channel *irc-eol*))))
   (irc-channels irc))
  (let ((password (irc-nick-password irc))
        (cfg-nick (irc-nickname irc))
        (nick (irc-username irc)))
    (if (and (string? password)
             (string-ci=? nick
                          cfg-nick))
        (irc-msg irc "NickServ" (string-append "IDENTIFY " password)))))

(set! *random-state* (random-state-from-platform))

(define (get-random-nick)
  (let ((number (+ 1000 (random 9000))))
    (string-append "ixirc"
                   (number->string number))))

(define-method (irc-nick-taken (irc <irc>) message params)
  (let ((new-nick (get-random-nick)))
    (<- (actor-id irc) 'irc-nick new-nick)))

(define-method (irc-msg (irc <irc>) to text)
  (format (irc-socket irc)
          "PRIVMSG ~a :~a~a" to text *irc-eol*))

(define-method (irc-nick (irc <irc>) nick)
  (slot-set! irc 'username nick)
  (format (irc-socket irc)
          "NICK ~a~a" nick *irc-eol*))

(define-method (irc-nick (irc <irc>) message nick)
  (irc-nick irc nick))
