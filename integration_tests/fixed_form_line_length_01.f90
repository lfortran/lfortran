c     Fixed-form source with lines up to column 132 (-ffixed-line-length-132)
      program fixed_form_line_length_01
      implicit none
      integer :: i, j
      character(len=200) :: s
      i = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17
      if (i /= 153) error stop
      s = 'this string is deliberately long so that it runs past column seventy-two'
      if (len_trim(s) /= 72) error stop
      j = 42                                                                                                                        + 1
      if (j /= 42) error stop
      s = 'abc
     &def'
      if (index(s, 'def') /= 122) error stop
      print *, i, j, index(s, 'def')
      end program
