#lang brag

comp: @attribute* @line*
line: instrument /COLON elements
instrument: ID
elements: @object*
object: @note-dur | @attribute | /MEASURE ;perhaps do something with this?
note-dur: note duration? 
note: NOTE @accidental*
duration: NUMBER
attribute: tempo | octave
tempo: /LPAREN (/TEMPO GLOBAL? NUMBER) /RPAREN
octave: /OCTAVE NUMBER | LBROCKET | RBROCKET
accidental: SHARP | FLAT
