(use-modules (ice-9 ftw))

(let ((files (scandir "."
                      (lambda (f) (string-prefix? "test-" f)))))
  (for-each (lambda (f)
              (load f))
            files))

(exit)
