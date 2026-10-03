      program goto_06
c Test: label in columns 1-5 with '0' in column 6 (initial line indicator)
c The parser must not include column 6 in the label value.
      integer :: x
      x = 0
      go to 12
      x = 1
 00120x = 2
      if (x /= 2) error stop
      end
