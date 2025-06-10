module list_tests
   implicit none
   real::eps = 1e-6

contains
   type(_lfortran_list(integer)) function fill_list_integer(size) result (ret)
      integer, intent(in):: size

      type(_lfortran_list(integer)) :: aarg = _lfortran_list_constant(0, 1, 2, 3, 4)
      integer::i
      do i = 0, size-1
         call _lfortran_list_append(aarg, i+5)
      end do

      ret = aarg
   end function


   subroutine test_list_01
      type(_lfortran_list(integer)) :: a
      type(_lfortran_list(real)) :: f = _lfortran_list_constant(1.0, 2.0, 3.0, 4.0, 5.0)
      integer::i
      a = fill_list_integer(10)

      do i = 0, 9
         call _lfortran_list_append(f, real(i+6))
      end do

      do i = 0, 14
         if ( abs(_lfortran_get_item(f, i) - real(_lfortran_get_item(a, i)) - 1.0) > eps ) error stop
      end do

      do i = 0, 14
         call _lfortran_set_item(f, i, _lfortran_get_item(f, i)+real(i))
      end do

      do i = 0, 14
         if ( abs(_lfortran_get_item(f, i) - real(_lfortran_get_item(a, i)) - real(i) - 1.0) > eps ) error stop
      end do
   end subroutine

   subroutine test_list_02()
      type(_lfortran_list(integer)) :: x = _lfortran_list_constant(1, 2)
      type(_lfortran_list(integer)) :: y
      integer::i

      do i = 3, 49
         call _lfortran_list_append(x, i+29)
      end do

      do i = 0, _lfortran_len(x)-1
         call _lfortran_list_append(y, _lfortran_get_item(x, i))
      end do

      do i = 0, _lfortran_len(x)-1
         if ( _lfortran_get_item(x, i) /= _lfortran_get_item(y, i) ) error stop
      end do
   end subroutine


   subroutine test_issue_1681
      ! issue 1681 from LP

      type(_lfortran_list(integer)) :: a = _lfortran_list_constant(2, 3, 4)
      a = _lfortran_list_constant(1, 2, 3)


      if ( _lfortran_len(a) /= 3 .or. _lfortran_get_item(a, 0) /= 1 .or. _lfortran_get_item(a, 1) /= 2 .or. _lfortran_get_item(a,2) /= 3 ) error stop


      a = _lfortran_list_constant(2)
      if ( _lfortran_len(a) /= 1 .or. _lfortran_get_item(a, 0) /= 2 ) error stop
   end subroutine

   subroutine tests()
      call test_list_01()
      call test_list_02()
      ! Removed negative indexing test
      call test_issue_1681()
   end subroutine
end module

program test_list_01_
   use list_tests
   implicit none

   call tests
end program
