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

(define (group c)
   (cond [(and (list? c) (equal? (car c) 'full)) (list (map val (cdr c)) (map typ (cdr c)))]
         [else c]))
(define (infix c l n)
  (if (empty? c) n
      (if (equal? (car c) "->")
          (infix (cddr c) (car c) (append (list "->") (list l (cadr c))))
          (infix (cdr c) (car c) (push n (car c))))))

(define (main)
  (let ([c (map group (process-line (map lex (string-split-spec (read-line))) '()))])
    (write (infix c '() '())))
  (main))

(main)