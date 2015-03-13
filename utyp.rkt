#lang racket
(require "parapp.rkt")

(define ret? third) (define typ second) (define val first)

(define (push~ stk s)
  (cond [(string=? (typ s) "Symbol") (if (and (not (empty? stk)) (list? (pop stk)) (not (empty? (pop stk)))
                                              (equal? (car (pop stk)) 'prog))
                                         (push (ret-pop stk) (push (pop stk) s))
                                         (push stk s))]
        [(string=? (typ s) "opn") (push stk (list 'prog))]
        [(string=? (typ s) "clos") (push (ret-pop stk) (append (list 'full) (cdr (pop stk))))]
        [(string=? (typ s) "app") (push stk (val s))]
        [else (push stk s)]))

(define (process-line s stk)
  (if (empty? s) stk (process-line (cdr s) (push~ stk (car s)))))

(define (out-item c)
  (if (list? (val c)) 
      (begin (display "( ") (map (λ (x) (display "~a " x)) (val c)) (display ") : ( ")
             (map (λ (x) (display "~a " x)) (typ c)) (displayln ")"))
      (displayln "~a : ~a" (val c) (typ c))))

(define (main)
  (let ([c (process-line (map lex (string-split-spec (read-line))) '())])
    (write c))
  (main))

(main)