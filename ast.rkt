#lang racket/base

(require lens racket/match)
(provide (all-defined-out))

(struct comp [attributes lines] #:transparent)
;; a line is a dict from Instrument to List of attribute/note/rest
;; an Instrument is a string or a (cons string string)
(define (voice-instrument v)
  (match v
    [(cons inst _) inst]
    [_ v]))

(define (voice-name v)
  (match v
    [(cons _ name) name]
    [_ v]))

(struct chord [elements])
(struct note [pitch accidental] #:transparent)
(struct rest [] #:transparent)

(struct attribute [global?] #:transparent)
;; attributes
(struct key-sig [sig])
(struct duration attribute [len] #:transparent)
(struct octave attribute [num] #:transparent)
(struct panning attribute [pan])
(struct quantization attribute [quant])
(struct tempo attribute [bpm] #:transparent)
(struct track-volume attribute [volume])
(struct transposition attribute [semitones])
(struct volume attribute [volume] #:transparent)

(struct store [last time key volume octave quantization panning tempo duration] #:transparent)
(define-struct-lenses store)

