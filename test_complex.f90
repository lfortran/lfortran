      subroutine sub1(a, n)
      integer n
      real a(*)
      call sub2(a(1), n)
      end subroutine

      subroutine sub2(x, m)
      integer m
      real x(*)
      x(1) = 1.0
      end subroutine
