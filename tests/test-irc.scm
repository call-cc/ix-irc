(define-module (tests test-irc)
  #:use-module (srfi srfi-64)
  #:use-module (ix irc)
  #:use-module (ice-9 receive))

(test-begin "ix-irc")

(define-syntax-rule (%import var)
  (define var
    (@@ (ix irc) var)))

(test-begin "Test is-prefix-msg?")

(%import is-prefix-msg?)

(define (test-is-prefix-msg? test-name expected str)
  (test-equal test-name expected (is-prefix-msg? str)))

(test-is-prefix-msg?
 "Test with empty string"
  #f
  "")

(test-is-prefix-msg?
 "Test with a single colon"
 #t
 ":")

(test-is-prefix-msg?
 "Test with a normal string"
 #t
 ":foo")

(test-is-prefix-msg?
 "Test with only colons"
 #t
 ":::::::")

(test-is-prefix-msg?
 "Test with no colons"
 #f
 "foobar")

(test-is-prefix-msg?
 "Test with colon at the end"
 #f
 "foobar:")

(test-is-prefix-msg?
 "Test with embedded colons"
 #f
 "f:o:o:b:a:r")

(test-end)

(test-begin "Test remove-prefix-colon")

(%import remove-prefix-colon)

(define (test-remove-prefix-colon test-name expected str)
  (test-equal test-name expected (remove-prefix-colon str)))

(test-remove-prefix-colon
 "Test with empty string"
 ""
 "")

(test-remove-prefix-colon
 "Test with non-prefix parameter of a single word"
 "foo"
 "foo")

(test-remove-prefix-colon
 "Test with non-prefix parameter with multiple words"
 "foo bar baz"
 "foo bar baz")

(test-remove-prefix-colon
 "Test with prefix parameter of a single word"
 "bar"
 ":bar")

(test-remove-prefix-colon
 "Test with prefix parameter with multiple words"
 "foo bar baz"
 ":foo bar baz")

(test-end)

(test-begin "Test fix-colon-param")

(%import fix-colon-param)

(define (test-fix-colon-param test-name expected params)
  (test-equal test-name expected (fix-colon-param params)))

(test-fix-colon-param
 "Test with empty list"
 '()
 '())

(test-fix-colon-param
 "Test with 1 non-colon parameter"
 '("foo")
 '("foo"))

(test-fix-colon-param
 "Test with 2 non-colon parameters"
 '("foo" "bar")
 '("foo" "bar"))

(test-fix-colon-param
 "Test with several non-colon parameters"
 '("foo" "bar" "baz" "quux" "frob")
 '("foo" "bar" "baz" "quux" "frob"))

(test-fix-colon-param
 "Test with 1 colon parameter"
 '("foo")
 '(":foo"))

(test-fix-colon-param
 "Test with 2 colon parameters"
 '("foo bar")
 '(":foo" "bar"))

(test-fix-colon-param
 "Test with several colon parameters"
 '("foo bar baz quux frob")
 '(":foo" "bar" "baz" "quux" "frob"))

(test-fix-colon-param
 "Test with mixed parameters"
 '("foo" "bar" "baz quux frob")
 '("foo" "bar" ":baz" "quux" "frob"))

(test-end)

(test-begin "Test get-params")

(%import get-params)

(define (test-get-params test-name expected params)
  (test-equal test-name expected (get-params params)))

(test-get-params
 "Test with empty list"
 #f
 '())

(test-get-params
 "Test with 1 non-colon parameter"
 '("foo")
 "foo")

(test-get-params
 "Test with 2 non-colon parameters"
 '("foo" "bar")
 "foo bar")

(test-get-params
 "Test with several non-colon parameters"
 '("foo" "bar" "baz" "quux" "frob")
 "foo bar baz quux frob")

(test-get-params
 "Test with 1 colon parameter"
 '("foo")
 ":foo")

(test-get-params
 "Test with 2 colon parameters"
 '("foo bar")
 ":foo bar")

(test-get-params
 "Test with several colon parameters"
 '("foo bar baz quux frob")
 ":foo bar baz quux frob")

(test-get-params
 "Test with mixed parameters"
 '("foo" "bar" "baz quux frob")
 "foo bar :baz quux frob")

(test-end)

(test-begin "Test is-target-channel?")

(define (test-is-target-channel? test-name expected target)
  (test-equal test-name expected (is-target-channel? target)))

(test-is-target-channel? "Test with '&' prefix"
                         #t
                         "&foo")

(test-is-target-channel? "Test with '#' prefix"
                         #t
                         "#foo")

(test-is-target-channel? "Test with '+' prefix"
                         #t
                         "+foo")

(test-is-target-channel? "Test with '!' prefix"
                         #t
                         "!foo")

(test-is-target-channel? "Test with non-channel"
                         #f
                         "schemebot")

(test-end)

(test-begin "Test is-target-user?")

(define (test-is-target-user? test-name expected target)
  (test-equal test-name expected (is-target-user? target)))

(test-is-target-user? "Test with '&' prefix"
                      #f
                      "&foo")

(test-is-target-user? "Test with '#' prefix"
                      #f
                      "#foo")

(test-is-target-user? "Test with '+' prefix"
                      #f
                      "+foo")

(test-is-target-user? "Test with '!' prefix"
                      #f
                      "!foo")

(test-is-target-user? "Test with a nickname"
                      #t
                      "schemebot")

(test-end)

(test-begin "Test parse-line")

(%import parse-line)

(test-begin "Test prefix parsing")

(define (test-prefix test-name expected raw-line)
  (test-equal test-name expected (parse-line raw-line)))

(test-prefix "Prefix with cloaked hostname"
             "z0d!~z0d@unaffiliated/z0d"
             ":z0d!~z0d@unaffiliated/z0d PRIVMSG bot :hi")

(test-prefix "Prefix with non-cloaked hostname"
             "z0d!z0d@c-68-47-172-222.hsd1.tn.comcast.net"
             ":z0d!z0d@c-68-47-172-222.hsd1.tn.comcast.net PRIVMSG bot :hi")

(test-prefix "Prefix with server hostname"
             "tepper.freenode.net"
             ":tepper.freenode.net NOTICE * :*** Looking up your tepper...")

(test-prefix "Prefix with MODE change"
             "bot"
             ":bot MODE bot :+i")

(test-prefix "Long prefix"
             "freenode-connect!frigg@freenode/utility-bot/frigg"
             ":freenode-connect!frigg@freenode/utility-bot/frigg NOTICE z00d :Welcome to freenode. To protect the network all new connections will be scanned for vulnerabilities. This will not harm your computer, and vulnerable hosts will be notified.")

(test-prefix "Line without prefix"
             #f
             "PING :tepper.freenode.net")

(test-end)

(test-begin "Test command parsing")

(define (test-command test-name expected raw-line)
  (receive (ln-prefix ln-cmd ln-params)
      (parse-line raw-line)
    (test-equal test-name expected ln-cmd)))

(test-command "Test numeric command"
              "376"
              ":tepper.freenode.net 376 z00d :End of /MOTD command.")

(test-command "Test alphanumeric command"
              "MODE"
              ":tepper.freenode.net MODE #test +ns")

(test-command "Test command without prefix"
              "PING"
              "PING :tepper.freenode.net")

;; This should fail
;; (test-command "Test invalid numeric command"
;;               ":tepper.freenode.net 3761 z00d :End of /MOTD command."
;;               #f)

(test-end)

(test-begin "Test parameter parsing")

(define (test-parameters test-name expected raw-line)
  (receive (ln-prefix ln-cmd ln-params)
      (parse-line raw-line)
    (test-equal test-name expected ln-params)))

(test-parameters "Test parameters form 1"
                 '("*" "*** Checking Ident")
                 ":tepper.freenode.net NOTICE * :*** Checking Ident")

(test-parameters "Test parameters form 2"
                 '("z00d" "Welcome to the freenode Internet Relay Chat Network z00d")
                 ":tepper.freenode.net 001 z00d :Welcome to the freenode Internet Relay Chat Network z00d")

(test-parameters "Test parameters form 3"
                 '("z00d" "33" "IRC Operators online")
                 ":tepper.freenode.net 252 z00d 33 :IRC Operators online")

(test-parameters "Test parameters form 4"
                 '("z00d" "84419" "94264" "Current global users 84419, max 94264")
                 ":tepper.freenode.net 266 z00d 84419 94264 :Current global users 84419, max 94264")

(test-parameters "Test parameters with many parameters"
                 '("z00d" "Welcome to freenode. To protect the network all new connections will be scanned for vulnerabilities. This will not harm your computer, and vulnerable hosts will be notified.")
                 ":freenode-connect!frigg@freenode/utility-bot/frigg NOTICE z00d :Welcome to freenode. To protect the network all new connections will be scanned for vulnerabilities. This will not harm your computer, and vulnerable hosts will be notified.")

(test-parameters "Test parameters with empty prefix"
                 '("tepper.freenode.net")
                 "PING :tepper.freenode.net")

(test-end)

(test-end)

(test-end)
