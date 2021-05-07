#lang racket/base

(require lens)
(provide (all-defined-out))

(struct comp [attributes lines] #:transparent)
;; an instrument is a string or a (cons string string)

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

