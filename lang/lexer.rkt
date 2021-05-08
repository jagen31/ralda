#lang racket
(require brag/support)

(provide lex)

(define-tokens music
  (ID COLON NOTE REST MEASURE GLOBAL
   LPAREN RPAREN
   SHARP FLAT OCTAVE LBROCKET RBROCKET
   TEMPO  NUMBER SLASH EOF))

(define lexer
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
   ["/" (token-SLASH lexeme)]
   [(:or "a" "b" "c" "d" "e" "f" "g") (token-NOTE (string->symbol lexeme))]
   ["r" (token-REST lexeme)]
   ["|" (token-MEASURE lexeme)]
   ["!" (token-GLOBAL lexeme)]
   ["tempo" (token-TEMPO (string->symbol lexeme))]
   [(:+ alphabetic) (token-ID (string->symbol lexeme))]
   [(eof) (token-EOF lexeme)]
   [whitespace (return-without-pos (lexer input-port))]))

(define (lex src) (λ() (lexer src)))
