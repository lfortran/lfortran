C Fixed-form CRLF test with label  
      program test
      character(len=80) :: s
  10  continue
      s = 'hello
     * world'
      print *, s
      if (s(1:61) /= 'hello') error stop
      if (s(62:) /= ' world') error stop
      end
