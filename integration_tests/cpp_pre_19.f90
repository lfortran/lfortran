      program cpp_pre_19
      implicit none
#define SUBNAME sub
#define CALLIT call SUBNAME
      integer :: i, j, k
C     "
#ifndef TEST
      i = 1
#else
      i = 2
#endif
C     "
c     don't
#if 0
      j = 3
c     "disabled
#else
      j = 4
#endif
*     "
      CALLIT(k)
      if (i /= 1) error stop
      if (j /= 4) error stop
      if (k /= 5) error stop
      print *, i, j, k
      contains
      subroutine sub(x)
      integer, intent(out) :: x
      x = 5
      end subroutine
      end program
