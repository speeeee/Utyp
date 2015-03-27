#lang racket
(require racket/list)
(provide push pop ret-pop pop-n
         push-n lex get-tok find-fun
         cons-fun push-cons fcons? 
         full-cons? exec-cons fexists? 
         string-split-spec type? lit)

(struct lit (val typ))
(define val first) (define typ second)

; stack-based cell-based(?) partial application-based language

(define funs* (list (list "+" '(X Y) (list (list "out" 'id)))))
; (name (parameters) (predicate))

(define (push stk elt) (append stk (list elt)))
(define (pop stk) (car (reverse stk)))
(define (ret-pop stk) (reverse (cdr (reverse stk))))
(define (pop-n stk n) (list (take stk (- (length stk) n)) (drop stk (- (length stk) n))))
(define (push-n stk elts) (append stk elts))

(define (lex x) 
  (cond [(list? x) x]
        [(equal? (string-ref x 0) #\") (list x "Symbol" #f)]
        [(or (and (> (string-length x) 1) (equal? (string-ref x 0) #\-)
                  (char-numeric? (string-ref x 0)))
             (char-numeric? (string-ref x 0))) (list x "Symbol" #f)]
        [(or (equal? (string-ref x 0) #\()) (list x "opn" #f)]
        [(member x '(":" "->" "=" "`" "#=")) (list x "app" #f)]
        [(equal? (string-ref x 0) #\{) (list x "opn" #f)]
        [(equal? (string-ref x 0) #\)) (list x "clos" #f)]
        [(equal? (string-ref x 0) #\}) (list x "lclos" #f)]
        [else (list x "Symbol" #f)]))

(define (*get-tok* f lst) 
  (let ([c (read-char f)]) (displayln c)
    (if (and (not (empty? lst)) (equal? (first lst) #\"))
        (if (equal? c #\") (string lst) (*get-tok* f (append lst (list c))))
        (if (or (equal? c #\space) (equal? c eof)) (string lst) 
            (*get-tok* f (append lst (list c)))))))
(define (get-tok f) (lex (string (*get-tok* f '()))))

(define (find-fun s fs)
  (car (filter (lambda (x) (string=? s (car x))) fs)))
(define (cons-fun stk fs)
  (let ([f (find-fun (car (pop stk)) fs)])
    (append (list f) 
            (if (< (- (length stk) 1) (length (second f))) (ret-pop stk)
                (second (pop-n (ret-pop stk) (length (second f))))))))
(define (push-cons stk fs)
  (let ([f (find-fun (car (pop stk)) fs)] [g (cons-fun stk fs)])
    (if (< (- (length stk) 1) (length (second f))) (push '() g)
        (push (car (pop-n stk (+ 1 (length (second f))))) g))))
(define (fcons? f fs)
  (ormap (lambda (x) (equal? (car f) x)) fs))
(define (fexists? v t fs)
  (ormap (lambda (x) (and (equal?? (car x) v)
                          (equal?? (second x) t))) fs))
(define (type? v fs)
  (ormap (λ (x) (equal?? x v)) (map car fs)))
(define (equal?? a b) 
  (if (not (and (list? a) (list? b))) (equal? a b)
  (cond [(not (= (length a) (length b))) #f] [(empty? a) #t]
        [(and (list? (car b)) (list? (car a)) (equal?? (car a) (car b))) (equal?? (cdr a) (cdr b))]
        [(or (equal? (string-ref (car b) 0) #\_)
             (equal? (string-ref (car a) 0) #\_)) (equal?? (cdr a) (cdr b))]
        [(equal? (car a) (car b)) (equal?? (cdr a) (cdr b))]
        [else #f])))

(define (full-cons? f) (= (length (cdr f)) (length (second (car f)))))
(define (exec-cons stk fs)
  (push-n (push-n (ret-pop (push-cons stk fs)) (cdr (cons-fun stk fs))) (caddar (cons-fun stk fs))))

(define stk '())
(set! stk (push-n stk (list (list "2" 'lit) (list "1" 'lit) (list "+" 'id))))

(define (string-split-spec str)
  (filter (λ (x) (not (empty? (string->list x)))) (splt str '())))
  ;(splt str '()))
(define (splt str lst)
  (if (empty? (string->list str)) lst
      (splt (cadr (tok (string->list str) '())) (append lst (list (car (tok (string->list str) '())))))))

(define (tok str lst)
  (if (empty? str) (list (list->string lst) "")
    (let ([c (car str)])
      (if (and (not (empty? lst)) (equal? (car lst) #\"))
          (if (equal? c #\") (list (list->string (append lst (list c))) (list->string (cdr str)))
              (tok (cdr str) (append lst (list c))))
          (if (or (char-whitespace? c)) (if (empty? lst) (tok (cdr str) lst) (list (list->string lst) (list->string str)))
              (tok (cdr str) (append lst (list c))))))))

