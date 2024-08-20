#lang racket
(provide parse-dice-notation)
(provide (rename-out [parse-dice-notation-verbose roll]))
(provide (rename-out [parse-dice-notation-silent roll-silent]))

(define (return-with-display v s [silent #f]) (if (not silent)
                                                  (begin0 v (displayln (format s v)))
                                                  v))
(define (string-is-num? str) (empty? (remove* (string->list "0123456789")
                                              (string->list str))))
(define (roll n [silent #f])
  (return-with-display (+ 1 (random n))
                       (string-join (list "d" (number->string n) "->~a") "") silent))
(define (repeat-roll x dx [silent #f])
  (define (repeat-roll-iter x dx)
    (if (= x 1) (roll dx silent) (+ (repeat-roll-iter (- x 1) dx) (roll dx silent))))
  (return-with-display (repeat-roll-iter x dx) `"=> ~a" silent))
(define (proper-format? str) (empty? (remove* (string->list "0123456789d+-")
                                              (string->list str))))
(define (has-error? str)
  (or (if (not (proper-format? str))
          (begin0 #t (displayln "unrecognized character"))
          #f)
      (if (and (string-contains? str "+")
               (string-contains? str "-"))
          (begin0 #t (displayln "cannot parse that order of operations."))
          #f)))

(define (parse-dice-notation dice-string [silent #f])
  (letrec ([split-with-operator
            (lambda (operator name-string)
              (return-with-display
               (apply operator (map parse-dice-iter
                                    (string-split dice-string name-string)))
               `"total: ~a" silent))]
           [parse-dice-iter
            (lambda (dice-string)
              (cond [(not (non-empty-string? dice-string)) (roll 20 silent)]
                    [(string-is-num? dice-string) (string->number dice-string)]
                    [(string-contains? dice-string "+") (split-with-operator + "+")]
                    [(string-contains? dice-string "-") (split-with-operator - "-")]
                    [(string-contains? dice-string "d")
                     (let ([dice-string-pair (string-split dice-string "d")])
                       (cond ((= 2 (length dice-string-pair))
                              (repeat-roll (string->number (first dice-string-pair))
                                           (string->number (last dice-string-pair)) silent))
                             ((= 1 (length dice-string-pair))
                              (roll (string->number (first dice-string-pair)) silent))))]
                    [else (error "something went wrong")]))])

    (unless (has-error? dice-string) (parse-dice-iter dice-string))))

(define (parse-dice-notation-silent dice-string)
  (parse-dice-notation dice-string #t))
(define (parse-dice-notation-verbose dice-string)
  (parse-dice-notation dice-string #f))
