#lang racket

(require "dice-parser.rkt")

(define (io-loop process-input)
  (letrec ([prompt "> " ]
           [get-input (lambda () (begin (display prompt)
                                        (read-line (current-input-port) 'any)))]
           [input (get-input)])
    (if (string=? input "exit") (exit)
        (begin (process-input input)
               (io-loop process-input)))))

(define (main) (if (terminal-port? (current-input-port))
                    (io-loop parse-dice-notation)
                    '()))
(main)
