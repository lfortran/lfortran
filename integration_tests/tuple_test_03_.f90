module tuple_test_03_mod
   implicit none
   real :: eps = 1e-12

contains
   subroutine tests()
      _lfortran_tuple(integer, real, character(len=:)) :: t1

      t1 = _lfortran_tuple_constant(1, 2.0, "3")
      integer :: x
      real :: y
      character(len=:) :: s

      x = _lfortran_get_item(t1, -3)
      y = _lfortran_get_item(t1, -2)
      s = _lfortran_get_item(t1, -1)

      if (x /= 1) error stop
      if (abs(y - 2.0) > eps) error stop
      if (s /= "3") error stop
   end subroutine
end module

program run_tuples
   use tuple_test_03_mod
   call tests()
end program
