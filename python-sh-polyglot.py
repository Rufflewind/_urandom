#!/bin/sh
END_OF_SCRIPT=None

''':' <<'END_OF_SCRIPT'''
print('python: begin')
END_OF_SCRIPT

''':'
echo 'sh: begin'
python <<'END_OF_SCRIPT'''

print('hi world~')

END_OF_SCRIPT

''':'
echo 'sh: end'
':'''

''':' <<'END_OF_SCRIPT'''
print('python: end')
END_OF_SCRIPT
