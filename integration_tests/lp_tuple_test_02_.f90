module tuple_test_02_mod
   implicit none
   real :: eps = 1e-12

contains

   function set_tuple(a, b) result(t)
      integer, intent(in) :: a
      real, intent(in) :: b
      type(_lfortran_tuple(integer, real, character(len=:), complex)) :: t
      character(len=:) :: s

      s = ""
      t = _lfortran_tuple_constant(a, b, s, cmplx(a, b))
   end function

   function merge_tuple(a, b) result(c)
      type(_lfortran_tuple(integer, real, character(len=:), complex)), intent(in) :: a, b
      type(_lfortran_tuple(integer, real, character(len=:), complex)) :: c
      integer :: a0, b0
      real :: a1, b1
      character(len=:) :: s
      complex :: a3, b3

      a0 = _lfortran_get_item(a, 0)
      b0 = _lfortran_get_item(b, 0)
      a1 = _lfortran_get_item(a, 1)
      b1 = _lfortran_get_item(b, 1)
      a3 = _lfortran_get_item(a, 3)
      b3 = _lfortran_get_item(b, 3)

      s = trim(to_string(a0)) // trim(to_string(b0))

      c = _lfortran_tuple_constant(a0 + b0, a1 + b1, s, a3 + b3)
   end function

   subroutine f()
      type(_lfortran_tuple(integer, real, character(len=:), complex)) :: t1, t2
      integer :: i
      real :: j
      integer :: x
      real :: y
      character(len=:) :: s
      complex :: z

      t1 = set_tuple(0, 0.0)
      do i = 0, 10
         j = real(i)
         t2 = set_tuple(i, j)
         t1 = merge_tuple(t1, t2)
      end do

      x = _lfortran_get_item(t1, 0)
      y = _lfortran_get_item(t1, 1)
      s = _lfortran_get_item(t1, 2)
      z = _lfortran_get_item(t1, 3)

      if (x /= 55) error stop
      if (abs(y - 55.0) > eps) error stop
      if (s /= "4510") error stop
      if (abs(z - cmplx(55.0, 55.0)) > eps) error stop

      print *, x, y, s, z
   end subroutine

   function g_check(x, y) result(res)
      type(_lfortran_tuple(integer, integer)), intent(in) :: x, y
      logical :: res
      res = _lfortran_get_item(x, 0) == _lfortran_get_item(y, 0)
   end function

   subroutine test_issue_1348()
      type(_lfortran_tuple(integer, integer)) :: a11, b11
      a11 = _lfortran_tuple_constant(1, 2)
      b11 = _lfortran_tuple_constant(1, 2)
      if (.not. g_check(a11, b11)) error stop
   end subroutine

   subroutine tests()
      call f()
      call test_issue_1348()
   end subroutine

   pure function to_string(i) result(s)
      integer, intent(in) :: i
      character(len=:) :: s
      character(len=32) :: buffer
      write(buffer, '(I0)') i
      s = trim(buffer)
   end function

end module

program run_tuples
   use tuple_test_02_mod
   call tests()
end program
