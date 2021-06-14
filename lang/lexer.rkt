#lang racket
(require brag/support syntax/parse/define)

(provide lex color-lex)

(define-tokens music
  (ID COLON NOTE REST MEASURE GLOBAL
   LPAREN RPAREN STRING
   SHARP FLAT OCTAVE LBROCKET RBROCKET SLASH
   TEMPO NUMBER WHITESPACE))

(define lexe
  (lexer-src-pos
   ["+" (token-SHARP lexeme)]
   ["-" (token-FLAT lexeme)]
   ["<" (token-LBROCKET lexeme)]
   [">" (token-RBROCKET lexeme)]
   ["(" (token-LPAREN lexeme)]
   [")" (token-RPAREN lexeme)]
   ["o" (token-OCTAVE lexeme)]
   [":" (token-COLON lexeme)]
   [(:+ numeric) (token-NUMBER (string->number lexeme))]
   [(:or "a" "b" "c" "d" "e" "f" "g") (token-NOTE (string->symbol lexeme))]
   ["r" (token-REST lexeme)]
   ["|" (token-MEASURE lexeme)]
   ["!" (token-GLOBAL lexeme)]
   ["/" (token-SLASH lexeme)]
   [(:: #\" (:* (:~ #\")) #\") (token-STRING lexeme)]
   ["tempo" (token-TEMPO (string->symbol lexeme))]
   [(:: alphabetic alphabetic (:+ (:or alphabetic numeric "-" "_")))
    (token-ID (string->symbol lexeme))]
   [(eof) (void)]
   [whitespace (token 'WHITESPACE lexeme #:skip? #t)]))

;; from
;; https://github.com/soegaard/minipascal/blob/master/minipascal/mini-pascal-lexer.rkt
(define (syn-val a b c d e)
  (values a ; string with matching text
          b ; symbol in '(comment white-space no-color eof)
          c ; symbol in '(|(| |)| |[| |]| |{| |}|) or #f.
          (position-offset d)    ; start pos
          (max                   ; end pos
           (position-offset e)
           (+ (position-offset d) 1))))

(define-simple-macro (my-values l r)
  (syn-val lexeme l r start-pos end-pos))
(define color-lexe
  (lexer
   ["+" (my-values 'constant #f)]
   ["-" (my-values 'constant #f)]
   ["<" (my-values 'symbol #f)]
   [">" (my-values 'symbol #f)]
   ["(" (my-values 'parenthesis '|(|)]
   [")" (my-values 'parenthesis '|)|)]
   ["o" (my-values 'constant #f)]
   [":" (my-values 'symbol #f)]
   [(:+ numeric) (my-values 'constant #f)]
   [(:or "a" "b" "c" "d" "e" "f" "g") (my-values 'constant #f)]
   ["r" (my-values 'constant #f)]
   ["|" (my-values 'symbol #f)]
   ["!" (my-values 'keyword #f)]
   ["/" (my-values 'constant #f)]
   [(:: #\" (:* (:~ #\")) #\") (my-values 'constant #f)]
   ["tempo" (my-values 'keyword #f)]
   [(:: alphabetic alphabetic (:+ (:or alphabetic numeric "-" "_")))
    (my-values 'symbol #f)]
   [(eof) (my-values 'eof #f)]
   [whitespace (my-values 'white-space #f)]))

;; *sigh*, parser expects a function that produces generator from port,
(define (lex src)
  (define (next) (lexe src))
  next)

;; ...while drracket does not.
(define (color-lex)
  (define (next src) (color-lexe src))
  next)
