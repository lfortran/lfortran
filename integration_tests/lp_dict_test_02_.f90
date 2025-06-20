module dict_test_01_mod
   implicit none
   real :: eps = 1e-6

contains

   subroutine f()
      _lfortran_dict(integer, real) :: r
      integer :: i
      integer :: s
      s = 1000

      call _lfortran_set_item(r, 0, 1.1)
      do i=1000, 1000+s-1
         call _lfortran_set_item(r, i, real(i)/100.0 + 5.0)
      end do


      do i=1000, 1000+s-1
         if ( abs(_lfortran_get_item(r, i) - real(i)/100.0 - 5.0) > eps ) error stop
      end do

      if ( abs(_lfortran_get_item(r, 0) - 1.1) > eps ) error stop
      if ( _lfortran_len(r) /= 1001 ) error stop
   end subroutine

   subroutine test_issue_1348()

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
