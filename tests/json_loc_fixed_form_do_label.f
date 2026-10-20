C     The JSON `loc` of each labeled do loop must end at its terminal
C     statement, not at the statement after it.
      subroutine s(n, x)
      integer n, i, j
      real x(n)
      do 10 i = 1, n
        x(i) = 0.0
   10 continue
      do 20 i = 1, n
   20 x(i) = 1.0
      do 30 i = 1, n
      do 30 j = 1, 2
        x(i) = x(i) + 2.0
   30 continue
      do 40 i = 1, n
        x(i) = 3.0
   40 end do
      do 50 i = 1, n
   50 x(i) = 4.0; x(1) = 5.0
      end subroutine
