      program format_hollerith_03
      implicit none
c     The text of an nH edit descriptor in a fixed-form FORMAT statement
c     is character data: its blanks, case and quotes are kept verbatim.
      character(len=72) :: s
      character(len=20) :: r(2)
      integer :: n
      n = 3
      write (s, 10)
      if (s /= ' A b  C d E') error stop 1
      write (s, 20)
      if (s /= ' ab C') error stop 2
      write (s, 30) n
      if (s /= ' X =  3') error stop 3
      write (r, 40)
      if (r(1) /= 'A B') error stop 4
      if (r(2) /= ' C D') error stop 5
      write (s, 50)
      if (s /= 'A''B') error stop 6
      write (s, 60)
      if (s /= 'SAY "X"') error stop 7
      write (s, 70) n
      if (s /= ' THEIR ORDERS ARE  3') error stop 8
c     A Hollerith that reaches column 72 and continues on the next line
      write (s, 80) n
      if (s(1:26) /= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ') error stop 9
      if (s(27:52) /= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ') error stop 10
      if (s(53:63) /= '12345678  3') error stop 11
c     A short line is padded with blanks to column 72
      write (s, 90) n
      if (s(1:3) /= 'ABC') error stop 12
      if (len_trim(s) /= 52) error stop 13
      if (s(51:52) /= ' 3') error stop 14
      print *, 'ok'
   10 format(11H A b  C d E)
   20 format(   5H ab C)
   30 format(1X,4HX = ,I2)
   40 format(3HA B/,4H C D)
   50 format(3HA'B)
   60 format(7HSAY "X")
   70 format(1X,
     1       16HTHEIR ORDERS ARE, I3)
   80 format(60HABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ1234
     15678,I3)
   90 format(50HABC
     1,I2)
      end program
