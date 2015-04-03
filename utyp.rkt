#lang racket
(require "parapp.rkt")

;( 1 2 ) { Int }

(define ret? third) (define typ second) (define val first)
(define cop (current-output-port))
(define (popp stk) (pop (pop stk)))

(define out first) (define in second)
;TODO: add facts.
(define funs* (list ; Output Input
                    (list "Int" "Symbol")
                    (list "String" "Symbol")
                    (list "Char" "Symbol")))

(define (comp-infix x ls)
  (if (empty? ls) x 
      (comp-infix (infix x (pop ls) '()) (ret-pop ls))))

(define (push~ stk s) ;(displayln stk) (displayln s)
  (cond [(string=? (typ s) "Symbol") (if (and (not (empty? stk)) (list? (pop stk)) (not (empty? (pop stk)))
                                              (equal? (car (pop stk)) 'prog))
                                         (push (ret-pop stk) (push~ (pop stk) s))
                                         (push stk s))]
        [(and (not (empty? stk)) (list? (pop stk)) (not (empty? (pop stk))) (list? (popp stk))
              (equal? (car (popp stk)) 'prog)) (push (ret-pop stk) (push~ (pop stk) s))]
        [(string=? (typ s) "opn") (if (and (not (empty? stk)) (list? (pop stk)) (not (empty? (pop stk)))
                                           (equal? (car (pop stk)) 'prog)) 
                                      (push (ret-pop stk) (push (pop stk) 
                                                                (if (string=? (val s) "('") (list 'prog (list "Group" "Symbol" #f) (list 'prog))
                                                                    (list 'prog))))
                                      (push stk (if (string=? (val s) "('") (list 'prog (list "Group" "Symbol" #f) (list 'prog)) (list 'prog))))]
        [(string=? (typ s) "clos") (cond #;[(string=? (val s) "')") (push (ret-pop stk) (group (append (list 'full) (cdr (pop stk)))))]
                                         [else (push (ret-pop stk) (group (append (list 'full) (cdr (pop stk)))))])]
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
   (cond [(and (list? c) (equal? (car c) 'full)) (list (map val (cdr c)) (map typ (cdr c)))
                                                 #;(list (append (list "Group") (list (map val (cdr c))))
                                                       (append (list "Group") (list (map typ (cdr c)))))]
         [else c]))
(define (infix c l n)
  (if (empty? c) n
      (if (equal? (car c) l)
          (infix (cddr c) l (push (ret-pop n) (append (list l) (list (pop n) (cadr c)))))
          (infix (cdr c) l (push n (car c))))))
(define (fun-app stk n)
  (if (empty? stk) n
      (if (type? (val (car stk)) funs*)
          (if (and (list? (car stk)) (not (empty? n)) (list? (pop n))
                   (fexists? (val (car stk)) (typ (pop n)) funs*))
              (fun-app (cdr stk) (push (ret-pop n) (list (pop n) (val (car stk)))))
              (begin (printf "ERROR: conversion `~a -> ~a' does not exist.~n"
                             (typ (pop n)) (val (car stk))) '()))
          (fun-app (cdr stk) (push n (car stk))))))
(define (mk-funs stk n)
  (if (empty? stk) n
      (if (or (equal? (caar stk) "=")
              (equal? (caar stk) "#=")) (begin (when (equal? (caar stk) "=") (fprintf cop "yes"))
                                               (set! funs* (push funs* (list (val (third (cadar stk))) (val (second (cadar stk))))))
                                               (mk-funs (cdr stk) n))
          (mk-funs (cdr stk) (push n (car stk))))))

#;(define (out-c n)
  (cond [(not (list? (val n))) (fprintf cop "~a;~n" (val n))]
        [(equal?? (typ n) (list "Group" "_a")) (begin (map (λ (x) ()) (ret-pop (val n))))]))

#;(define (trans->c n)
  (cond [(not (list? (val n))) (format "~a" (val n))]
        [(equal?? (typ n) (list "Group" "_a")) (let ([o (open-output-string)])
                                                 (fprintf o ))]))

(define (parse x)
  (mk-funs (fun-app (comp-infix (process-line 
                                 (map lex (flatten (map (λ (y) (if (string=? y "')") (list ")" ")") y)) (string-split-spec x)))) '()) (list "#=" "=" "->")) '()) '()))
(define (main)
  (let ([c (parse (read-line))])
    (write c)
    #;(when (not (empty? c)) (out-c (car c))))
  (main))

(main)