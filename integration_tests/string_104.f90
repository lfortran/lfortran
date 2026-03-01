program fmtbuga
  character fmt*10, line*80, letters*26
  integer i
  letters = 'abcdefghijklmnopqrstuvwxyz'
  fmt = '(99(1X,A))'
  write(line,fmt) (letters(i:i), i=1,26)
  if (trim(line) /= ' a b c d e f g h i j k l m n o p q r s t u v w x y z') error stop 'wrong output'
  print "(A)", trim(line)
  print *, 'Test passed'
end program fmtbuga