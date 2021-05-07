#lang brag

comp: @line*
line: instrument /COLON elements
instrument: ID
elements: @object*
object: @note-dur | octave | /MEASURE ;perhaps do something with this?
note-dur: note duration? 
note: NOTE @accidental*
duration: NUMBER
octave: /OCTAVE NUMBER | LBROCKET | RBROCKET
accidental: SHARP | FLAT
