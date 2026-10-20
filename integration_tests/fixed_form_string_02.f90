      program fixed_form_string_02
      implicit none
      character(len=80) :: s
c     The next line ends at column 15; blanks fill columns 16 to 72
      s = '[abc
     &def]'
      print *, len_trim(s)
      print *, s
      if (len_trim(s) /= 65) error stop
      if (s(1:61) /= '[abc') error stop
      if (s(62:) /= 'def]') error stop
      end program
