# This example shows the basics on how to define a part and add notes to it

:part # this begins a part definition
  :name Melodia # this defines part name
  :id 3 # this defines part id. It is used to reference this part in notes sections
  :clef g # this defines the clef
  :key f # this defines the number of sharps or flats in the key signature
  :time 4 4 # this defines the time signature. First, numerator, then denominator
part: # this ends a part definition

:notes 3 # this defines notes for part with id 3, defined above
  # measures are separated by |
  t:intro [d.m9] r4 r r r # 'r4' represents a rest of quarter note duration 
  |
  [d.m9] r4 r r r # '[d.m9]' represents a D minor 9 chord
notes: # this ends notes for part with id 3

# multiple notes sections can be defined for a single part
:notes 3
  # o+ changes increases current octave by 1
  # similarly, o- decreases current octave by 1
  t:verse [d.m9] a8 o+ c c c c4. d8 |
  c1 | 
  [d.m9] o- a8 o+ c c c c4. d8 |
  c1 | 
notes:

:notes 3
  # elements inside notes section should be separated by at least one space
  [g.m9]  d8  f  f  f  f4.  g8 
  |
  f2. r8 o- bf8 
  |
  [e.m9(11)] a8 a a a~ a4. o+ d8 
  |
  [ef.7(#11)] o- a8 a a4 a4.~ a16 a32 o+ c | # you can use comments to "disable" music c |
  [d.m9] o- g1 |  
notes:

# end barline is attached to the last measure automatically
