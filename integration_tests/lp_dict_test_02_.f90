module dict_test_02_mod
   implicit none
   real :: eps = 1e-5

contains

   subroutine test_dict()
      _lfortran_dict(integer, real) :: rollnumber2cpi
      integer :: i
      integer :: size, size1
      size = 7000
      size1 = size/7 + 1

      rollnumber2cpi = _lfortran_dict_constant(0, 1.1)
      do i=1000, 1000 + size - 1, 7
         call _lfortran_set_item(rollnumber2cpi, i, real(i)/100.0 + 5.0)
      end do


      do i=1000, 1000 + size/2 - 1, 7
         if ( abs(_lfortran_pop(rollnumber2cpi, i) - real(i)/100.0 - 5.0) > eps ) error stop
         size1 = size1 - 1
         if ( _lfortran_len(rollnumber2cpi) /= size1 ) error stop
      end do

      do i=1000, 1000 + size/2 - 1, 7
         call _lfortran_set_item(rollnumber2cpi, i, -real(i)/100.0 - 5.0)
      end do

      do i=1000, 1000 + size/2 - 1, 7
         if ( abs(_lfortran_get_item(rollnumber2cpi, i) + real(i)/100.0 + 5.0) > eps ) error stop
      end do

      do i=1000, 1000 + size - 1, 7
         call _lfortran_set_item(rollnumber2cpi, i, -real(i)/100.0 - 5.0)
      end do

      do i=1000, 1000 + size - 1, 7
         if ( abs(_lfortran_get_item(rollnumber2cpi, i) + real(i)/100.0 + 5.0) > eps ) error stop
      end do
   end subroutine

   subroutine tests()
      call f()
   end subroutine

end module

program run_tuples
   use dict_test_02_mod
   call tests()
end program
