      module fixed_form_semicolon_02_m
      implicit none
      type t; integer a; end type
      type v; integer :: c, d; end type v
      end module

      subroutine fixed_form_semicolon_02_set
      integer m, n, p
      common /fixed_form_semicolon_02_b/ m, n
      common p; m = 7; n = 8; p = 1; end

      subroutine fixed_form_semicolon_02_get(k)
      integer k, m, n
      common /fixed_form_semicolon_02_b/ m, n; k = 10*m + n; end

      program fixed_form_semicolon_02
      use fixed_form_semicolon_02_m
      implicit none
      integer :: i, k, m, n
      character(len=8) :: s
      type(t) :: x
      type(v) :: y
      common /fixed_form_semicolon_02_b/ m, n
      x%a = 1; y%c = 2; y%d = 3
      if (x%a + y%c + y%d /= 6) error stop 1
      k = 0
      write(s, 100) 5
  100 format(i3); k = k + 10
      if (k /= 10) error stop 2
      if (s /= '  5') error stop 3
      k = 0
      do i = 1, 2
      write(s, 200) i
  200 format(i2, ';'); end do; k = k + 1
      if (k /= 1) error stop 4
      if (s /= ' 2;') error stop 5
      call fixed_form_semicolon_02_set
      if (m /= 7 .or. n /= 8) error stop 6
      call fixed_form_semicolon_02_get(k)
      if (k /= 78) error stop 7
      print *, k
      end program
