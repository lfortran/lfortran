module dict_test_01_mod
   implicit none
   real :: eps = 1e-6

contains

   subroutine f()
      _lfortran_dict(integer, real) :: rollnumber2cpi
      integer :: i
      integer :: size
      size = 1000

      call _lfortran_set_item(rollnumber2cpi, 0, 1.1)
      do i=1000, 1000+size-1
         call _lfortran_set_item(rollnumber2cpi, i, real(i)/100.0 + 5.0)
      end do


      do i=1000, 1000+size-1
         if ( abs(_lfortran_get_item(rollnumber2cpi, i) - real(i)/100.0 - 5.0) > eps ) error stop
      end do

      if ( abs(_lfortran_get_item(rollnumber2cpi, 0) - 1.1) > eps ) error stop
      if ( _lfortran_len(rollnumber2cpi) /= 1001 ) error stop
   end subroutine

   subroutine test_issue_1348()
      if ( _lfortran_len(_lfortran_dict_constant(1, 2, 1, 3, 4, 5)) /= 2 ) error stop
      if ( _lfortran_len(_lfortran_dict_constant(1, 2, 1, 3, 1, 5)) /= 1 ) error stop
   end subroutine

   subroutine tests()
      call f()
      call test_issue_1348()
   end subroutine

end module

program run_tuples
   use dict_test_01_mod
   call tests()
end program
