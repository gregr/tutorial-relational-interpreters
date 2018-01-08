#lang racket

(require "mk.rkt")

;(define (append l s)
  ;(if (null? l)
    ;s
    ;(cons (car l) (append (cdr l) s))))

;(define (append l s)
  ;(cond
    ;((null? l) s)
    ;(else (cons (car l) (append (cdr l) s)))))

(define (append l s)
  (match l
    ('() s)
    (`(,a . ,d) (cons a (append d s)))))

;(define (appendo l s ls)
  ;(conde
    ;((== '() l) (== s ls))
    ;((fresh (a d ds)
       ;(== `(,a . ,d) l)
       ;(appendo d s ds)
       ;(== `(,a . ,ds) ls)))))

(define (appendo l s ls)
  (conde
    ((== '() l) (== s ls))
    ((fresh (a d ds)
       (== `(,a . ,d) l)
       (== `(,a . ,ds) ls)
       (appendo d s ds)))))


(define (lookup x env)
  (match-let ((`((,y . ,v) . ,rest) env))
    (if (eqv? y x)
      v
      (lookup x rest))))

;(define (lookup x env)
  ;(match-let ((`((,y . ,v) . ,rest) env))
    ;(cond
      ;((eqv? y x) v)
      ;(else (lookup x rest)))))

(define (lookupo x env val)
  (fresh (y v rest)
    (== `((,y . ,v) . ,rest) env)
    (conde
      ((== y x) (== v val))
      ((=/= y x) (lookupo x rest val)))))


(define (not-in-env x env)
  (match env
    ('() #t)
    (`((,y . ,v) . ,rest)
      (and (not (eqv? y x))
           (not-in-env x rest)))))

(define (not-in-envo x env)
  (conde
    ((== '() env))
    ((fresh (y v rest)
       (== `((,y . ,v) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))


(define closure-tag (gensym "closure"))


(define (evaluate expr env)
  (match expr
    (`(quote ,v) v)

    ((? symbol?)
     (lookup expr env))

    (`(cons ,ea ,ed)
      `(,(evaluate ea env) . ,(evaluate ed env)))

    (`(lambda (,(? symbol? x)) ,body)
      `(,closure-tag ,x ,body ,env))

    (`(,rator ,rand)
      (match-let ((`(,closure-tag ,x ,body ,env^) (evaluate rator env))
                  (arg (evaluate rand env)))
        (evaluate body `((,x . ,arg) . ,env^))))))



(define (evaluate expr env)
  (define (unshadowed? name) (not-in-env name env))
  (match expr
    (`(quote ,v) v)

    ((? symbol?)
     (lookup expr env))

    (`(,(? unshadowed? 'cons) ,ea ,ed)
      `(,(evaluate ea env) . ,(evaluate ed env)))

    (`(,(? unshadowed? 'lambda) (,(? symbol? x)) ,body)
      `(,closure-tag ,x ,body ,env))

    (`(,rator ,rand)
      (match-let ((`(,closure-tag ,x ,body ,env^) (evaluate rator env))
                  (arg (evaluate rand env)))
        (evaluate body `((,x . ,arg) . ,env^))))))


(define (evaluate expr env)
  (define (unshadowed? name) (not-in-env name env))
  (match expr
    (`(quote ,v) v)

    ((? symbol?)
     (lookup expr env))

    (`(,(? unshadowed? 'cons) ,ea ,ed)
      `(,(evaluate ea env) . ,(evaluate ed env)))

    (`(,(? unshadowed? 'lambda) (,(? symbol? x)) ,body)
      `(,closure-tag ,x ,body ,env))

    (`(,rator ,rand)
      (match (evaluate rator env)
        (`(,closure-tag ,x ,body ,env^)
          (let ((arg (evaluate rand env)))
            (evaluate body `((,x . ,arg) . ,env^))))))))


(define (evaluate expr env)
  (define (unshadowed? name) (not-in-env name env))
  (match expr
    (`(quote ,v) v)

    ((? symbol?)
     (lookup expr env))

    (`(,(? unshadowed? 'cons) ,ea ,ed)
      `(,(evaluate ea env) . ,(evaluate ed env)))

    (`(,(? unshadowed? 'lambda) (,(? symbol? x)) ,body)
      `(,closure-tag ,x ,body ,env))

    (`(,rator ,rand)
      (match (evaluate rator env)
        (`(,closure-tag ,x ,body ,env^)
          (evaluate body `((,x . ,(evaluate rand env)) . ,env^)))))))



(define (evaluateo expr env val)
  (conde
    ((== `(quote ,val) expr)
     (not-in-envo 'quote env)
     (absento closure-tag val))

    ((symbolo expr)
     (lookupo expr env val))

    ((fresh (ea ed va vd)
       (== `(cons ,ea ,ed) expr)
       (== `(,va . ,vd) val)
       (absento closure-tag val)
       (not-in-envo 'cons env)
       (evaluateo ea env va)
       (evaluateo ed env vd)))

    ((fresh (x body)
       (== `(lambda (,x) ,body) expr)
       (== `(,closure-tag ,x ,body ,env) val)
       (symbolo x)
       (not-in-envo 'lambda env)))

    ((fresh (rator rand arg x body env^)
       (== `(,rator ,rand) expr)
       (evaluateo rator env `(,closure-tag ,x ,body ,env^))
       (evaluateo rand env arg)
       (evaluateo body `((,x . ,arg) . ,env^) val)))))


(define (evalo expr val)
  (evaluateo expr '() val))
