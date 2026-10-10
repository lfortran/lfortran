      program doloop_22
!     Nonblock DO constructs: DO <label> loops that end on a labelled
!     CONTINUE or action statement, alone or shared by nested loops
      implicit none
      integer :: i, j, k, n
      character(2) :: line(2)
      n = 0
      do 10 i = 1, 3
      do 10 j = 1, 3
      if (j == 2) go to 10
      n = n + 1
   10 continue
!     the loops above end here
      if (n /= 6) error stop
      k = 0
      do 20 i = 1, 2
      do 20 j = 1, 2
   20 k = k + i*j
      if (k /= 9) error stop
      n = 0
      do 30, i = 1, 4
      if (i == 3) go to 30
      n = n + i
   30 continue
      if (n /= 7) error stop
      n = 0
      do 40 i = 1, 5, 2
   40 n = n + i
      if (n /= 9) error stop
!     the terminal statement is a CALL, a WRITE, an IF or a PRINT
      n = 0
      do 50 i = 1, 3
   50 call add(n, i)
      if (n /= 6) error stop
      do 60 i = 1, 2
      do 60 j = 1, 2
   60 write (line(i)(j:j), '(i1)') i + j
      if (line(1) /= '23' .or. line(2) /= '34') error stop
      n = 0
      do 70 i = 1, 4
      do 70 j = 1, 2
      if (j == 2) go to 70
      n = n + 10
   70 if (i /= 2) n = n + i
      if (n /= 56) error stop
      do 80 i = 1, 2
   80 print *, i
      contains
      subroutine add(n, i)
      integer, intent(inout) :: n
      integer, intent(in) :: i
      n = n + i
      end subroutine add
      end program doloop_22
