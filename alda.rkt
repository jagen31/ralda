#lang racket
(require lens lens/data unstable/lens
         rsound rsound/envelope rsound/piano-tones)

(module+ test (require rackunit))

(struct comp [attributes lines] #:transparent)
(struct line [instruments elements] #:transparent)
(define-struct-lenses line)
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

(define ~> lens-thrush)

(define (hash-map-lens lens)
  (make-lens
   (λ(x) (lens-view/map lens x))
   (λ(tgts new-views) (lens-set/map lens tgts new-views))))

(define (lens-view/map lens tgts)
  (hash-map (λ(k v) (lens-view lens v)) tgts))

(define (lens-set/map lens tgts new-views)
  (for/hash ([(k v) (in-hash tgts)])
    (values k (lens-set lens v new-views))))

(define (make-piano tone len)
  (rs-mult (piano-tone tone) ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len)))

(define (midi->rsound tone len)
  (make-piano tone len))

(define (note->midi p a o)
  (+ (match p ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11])
           a
           (* 12 o) 12))

(define (tee v) (print v) v)
(define default-store (store #f 0 #f 50 4 100 0 120 4))

(define (compile c sto)
  (define init-store
    (match c
      [(comp attributes lines)
       ;; initialize the store
       (define sto* (for/fold ([sto sto])
                              ([(k _) (in-dict lines)])
                      (hash-set sto k default-store)))
       (foldl (curry interpret-attribute #f) sto* attributes)]))
  (define (compile lines sto)
    (define next-voice
      (for/fold ([vo #f] [s +inf.0] #:result vo)
                ([(k v) (in-hash sto)])
        (if (< (store-time v) s)
            (values k (store-time v))
            (values vo s))))
    (match next-voice
      [#f '()]
      [_
    (define-values (new-rsounds new-store)
      (match (hash-ref sto next-voice)
        [(store last time key vol o q pan tempo duration)
         (match last
           [#f (values '() sto)]
           [(attribute _) (values '() (interpret-attribute next-voice last sto))]
           [(note p a)
            (printf "here for: ~s, note is: ~s ~s ~s, ~s\n" next-voice p a o duration)
            (define csto (hash-ref sto next-voice))
            (define st (store-time csto))
            (define end (+ time (/ (* 2 (default-sample-rate) tempo) duration 60)))
            (define sto*
              (hash-set sto next-voice (store last end key vol o q pan tempo duration)))
            (values (list (list (midi->rsound (note->midi p a o) (- end st)) time))
                    sto*)])]))
         (define ln (hash-ref lines next-voice))
         (append new-rsounds
                 (match ln
                   [(cons expr more)
                    (compile (hash-set lines next-voice more)
                             (lens-set ((hash-ref-lens next-voice) . ~> . store-last-lens) new-store expr))]
                   [_ (compile (hash-remove lines next-voice) (hash-remove sto next-voice))]))]))
  (compile (comp-lines c) init-store))

(define (interpret-attribute voice a sto)
  (define line-lens
    (match a
      [(attribute #t) hash-map-lens]
      [_ (λ(lens) ((hash-ref-lens voice) . ~> . lens))]))
  (match a
    [(volume _ v) (lens-set (line-lens store-volume-lens) sto v)]
    [(tempo _ t) (lens-set (line-lens store-tempo-lens) sto t)]
    [(duration _ d) (lens-set (line-lens store-duration-lens) sto d)]
    [(octave _ o)
     (define f
       (match o ['up add1] ['down sub1] [n (λ(_) n)]))
     (lens-transform (line-lens store-octave-lens) sto f)]))

;(interpret-attribute "violin" (octave #f 20) (hash "violin" default-store))

#;(define sample-store
  (make-immutable-hash
   (list (cons (cons "violin" "violin-1")
               (store false 0 false 50 4 100 0 80 4))
         (cons (cons "violin" "violin-2")
               (store false 0 false 50 4 100 0 80 4))
         (cons "cello" (store false 0 false 50 4 100 0 80 4))
         (cons "viola" (store false 0 false 50 4 100 0 80 4)))))

;(interpret-attribute "violin" (octave #f 20) (hash "violin" default-store))

(define ex1
  (comp
   (list (tempo #t 80))
   (hash (cons "violin" "violin-1")
         (list (octave #f 4)
               (duration #f 2) (note 'f 0)
               (duration #f 4) (note 'g 0)
               (note 'a 0)
               (duration #f 2) (note 'b -1)
               (note 'a 0))
         (cons "violin" "violin-2")
         (list (octave #f 4)
               (duration #f 2) (note 'c 0)
               (duration #f 4) (note 'e 0)
               (note 'f 0)
               (duration #f 2) (note 'f 0)
               (note 'f 0))
         "viola"
         (list (octave #f 3)
               (duration #f 2) (note 'a 0)
               (octave #f 'up)
               (duration #f 4) (note 'c 0)
               (note 'c 0)
               (duration #f 2) (note 'd 0)
               (note 'c 0))
         "cello"
         (list (octave #f 3)
               (duration #f 2) (note 'f 0)
               (duration #f 4) (note 'c 0)
               (note 'f 0)
               (octave #f 'down)
               (duration #f 2) (note 'b -1)
               (octave #f 'up)
               (note 'f 0)))))

(play (assemble (compile ex1 (hash))))
