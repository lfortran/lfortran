module list_test_04_mod
   implicit none
contains

   subroutine test_list_01()
      type(_lfortran_list(integer)) :: x, y
      integer :: i, j
      real :: f

      do i = 0, 4
         j = i**2 + 10
         call _lfortran_list_insert(x, _lfortran_len(x), j)
      end do

      do i = 0, _lfortran_len(x)-1
         f = sqrt(real(_lfortran_get_item(x, i))-10.0)
         call _lfortran_list_append(y, int(f))
      end do

      do i = 0, _lfortran_len(x)-1
         call _lfortran_list_remove(x, i**2 + 10)
         call _lfortran_list_append(x, i)
      end do

      ! Final assertion
      do i = 0, _lfortran_len(y)-1
         if (_lfortran_get_item(x, i) /= _lfortran_get_item(y, i)) error stop
      end do
   end subroutine


   subroutine test_list_02()
      type(_lfortran_list(character(len=:))) :: x
      character(len=32) :: buffer
      character(len=:), allocatable :: s
      integer :: i

      do i = 1, 50
         write(buffer, '(I0)') i
         s = trim(buffer) // "_str"
         call _lfortran_list_append(x, s)
      end do

      do i = 1, 50
         write(buffer, '(I0)') i
         s = trim(buffer) // "_str"
         call _lfortran_list_remove(x, s)
      end do

      if (_lfortran_len(x) /= 0) error stop
   end subroutine


   subroutine tests()
      call test_list_01()
      call test_list_02()
   end subroutine

end module


program test_list_remove_main
   use list_test_04_mod
   implicit none

   call tests()
end program
