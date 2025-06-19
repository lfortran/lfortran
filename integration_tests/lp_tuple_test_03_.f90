module tuple_test_03_mod
   implicit none
   real :: eps = 1e-12

contains


   subroutine f()
      type(_lfortran_tuple(integer, real, character(len=:), complex)) :: t1, t2
      integer :: i
      real :: j
      integer :: x
      real :: y
      character(len=:), allocatable :: s
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
      _lfortran_tuple(integer, real, character(len=:), complex)
   end subroutine

   pure function to_string(i) result(s)
      integer, intent(in) :: i
      character(len=:), allocatable :: s
      character(len=32) :: buffer
      write(buffer, '(I0)') i
      s = trim(buffer)
   end function

end module

program run_tuples
   use tuple_test_03_mod
   call tests()
end program
