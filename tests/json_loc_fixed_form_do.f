C     The JSON `loc` of each loop must end at its `end do`/`enddo`,
C     and of each program unit at its `end` statement.
      subroutine s(n, x)
      integer n, i
      real x(n)
      do i = 1, n
        x(i) = 0.0
      end do
      do i = 1, n
        x(i) = 1.0
      enddo
      outer: do i = 1, n
        x(i) = 2.0
      end do outer
      do while (n > 5)
        n = n - 1
      end do
      do concurrent (i = 1:n)
        x(i) = 3.0
      enddo
      do i = 1, n
        x(i) = 4.0
      enddo; x(1) = 5.0
      end subroutine
      real function f(x)
      real x
      f = x
      end
