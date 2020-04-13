;; hello.scm
;; Author: Taylor Schmidt
;; This file contains learning resources/examples for Guile Scheme
#!/usr/local/bin/guile -s
!#

;; print to stdout
(display "Hello, world!")
(newline)

;; make a variable
(define hello "Hello")
(display hello)
(newline)

;; modify a variable's values
;; variables can hold any type of value
(set! hello 69)
(display hello)
(newline)

;; calling procedures
(string-length "hello")
;; chaining calls
(string-length (string-append "hell" "o"))

;; anonymous functions
(lambda (name address)
  (string-append "Name=" name ":Address=" address))

;; function/procedure definition
(define (make-combined-string name address)
  (string-append "Name=" name ":Address=" address))

;; local vars defined by let. only available in scope
(define (test-let-vars num)
  (let ((next 3))
    (+ num next)))
(display (test-let-vars 4))
(newline)

(make-combined-string "hello" "there")

;; local persistent variables can be defined within environments.
;; these vars are essentially "private" to any lambda defined within the env.
(define get-balance #f)
(define deposit #f)

;; in this env, the var balance is persisted between calls to both functions
;; and inaccessible to any outside code.
(let ((balance 0))
  (set! get-balance
	(lambda ()
	  balance))
  (set! deposit
	(lambda (amount)
	  (set! balance (+ balance amount))
	  balance)))

;; gets the command line args
(display (command-line))
(newline)
