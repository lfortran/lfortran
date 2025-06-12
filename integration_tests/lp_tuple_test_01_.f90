module tuple_test
   implicit none

contains

   subroutine f()
      type(_lfortran_tuple(integer, real, character(len=:), logical)) :: t1, t2
      type(_lfortran_tuple(integer, integer)) :: t3
      type(_lfortran_tuple(real, real)) :: t4

      integer :: a, b
      real :: af, bf

      t1 = _lfortran_tuple_constant(1, 2.0, "3", .true.)
      t2 = _lfortran_tuple_constant(2, 3.0, "3", .false.)

      if (_lfortran_get_item(t2, 0) - _lfortran_get_item(t1, 0) /= 1) error stop
      if (abs(_lfortran_get_item(t2, 1) - _lfortran_get_item(t1, 1) - 1.0) > 1e-12) error stop
      if (_lfortran_get_item(t1, 2) /= _lfortran_get_item(t2, 2)) error stop
      if (.not. (_lfortran_get_item(t1, 3) .or. _lfortran_get_item(t2, 3))) error stop

      t3 = _lfortran_tuple_constant(1, 2)
      t4 = _lfortran_tuple_constant(1.0, 2.0)

      a = _lfortran_get_item(t3, 0)
      b = _lfortran_get_item(t3, 1)
      af = _lfortran_get_item(t4, 0)
      bf = _lfortran_get_item(t4, 1)

      if (af /= real(a)) error stop
      if (bf /= real(b)) error stop
   end subroutine

end module

program main
   use tuple_test
   implicit none
   call f()
end program
