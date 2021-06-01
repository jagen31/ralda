#lang racket/base

(require ralda/lang/lexer ralda/lang/parser (prefix-in ast: ralda/ast)
         (for-syntax racket/base racket/list racket/match syntax/parse))
(provide (rename-out [alda:module-begin #%module-begin]
                     [alda:read-syntax read-syntax]
                     [alda:get-info get-info])
         #%datum
         comp instrument elements octave note duration tempo)

(define (alda:read-syntax path in)
  (datum->syntax #f `(module random ralda/lang/reader ,(parse path (lex in)))))

(define (alda:get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer) (color-lex)]
      [else default])))

(define-syntax alda:module-begin
  (syntax-parser
    [(_ body)
     #'(#%module-begin
        (define the-sound body)
        (provide the-sound))]))

(define-syntax comp
  (syntax-parser
    [(_ bodies ...)
     #:do [(define-values (attrs other)
             (splitf-at (syntax->list #'(bodies ...))
                        (λ(b) (syntax-parse b
                                [({~literal instrument} _ ...) #f] [_ #t]))))]
     #:with (attrs* ...) attrs
     #:with (other* ...) other
     #'(ast:comp (list attrs* ...) (hash other* ...))]))

(define-syntax instrument
  (syntax-parser
    [(_ id:id) #'(symbol->string 'id)]
    [(_ id:id name) #'(cons (symbol->string 'id) name)]))

(define-syntax elements
  (syntax-parser
    [(_ exprs ...) #'(list exprs ...)]))

(define-syntax octave
  (syntax-parser
    [(_ val)
     (match (syntax->datum #'val)
       [">" #'(ast:octave #f 'up)]
       ["<" #'(ast:octave #f 'down)]
       [_ #'(ast:octave #f val)])]))

(define-syntax note
  (syntax-parser
    [(_ pitch accidentals ...)
     #`(ast:note
        'pitch
        #,(for/sum ([a (syntax->list #'(accidentals ...))])
            (match (syntax->datum a) ["+" 1] ["-" -1])))]))

(define-syntax duration
  (syntax-parser
    [(_ n:number) #'(ast:duration #f n)]
    [(_ n:number d:number) #'(ast:duration #f (/ n d))]))

(define-syntax (tempo stx)
  (syntax-parse stx
    [(_ n:number) #'(ast:tempo #f n)]
    [(_ global n:number) #'(ast:tempo #t n)]))
