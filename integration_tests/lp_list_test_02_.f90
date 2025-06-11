module list_str_tests
   implicit none
   character(len=:), allocatable :: tmp_str
contains

   function fill_list_str(size) result(a)
      integer, intent(in) :: size
      type(_lfortran_list(character(len=:))) :: a
      integer :: i
      character(len=:), allocatable :: str_tmp

      a = _lfortran_list_constant("0_str", "1_str")

      do i = 0, size-1
         write(str_tmp, '(I0)') i + 2
         str_tmp = trim(str_tmp)//"_str"
         call _lfortran_list_append(a, str_tmp)
      end do
   end function


   subroutine test_list_01()
      type(_lfortran_list(character(len=:))) :: a, b
      character(len=:), allocatable :: string
      integer :: i
      character(len=:), allocatable :: str_tmp

      string = "string_"
      b = _lfortran_list_constant(string, string)
      a = fill_list_str(10)

      do i = 0, 9
         write(str_tmp, '(I0)') i + 2
         call _lfortran_list_append(b, trim(string) // trim(str_tmp))
      end do

      do i = 0, 11
         print *, _lfortran_get_item(a, i), _lfortran_get_item(b, i)
      end do

      call _lfortran_set_item(a, 11, "no_str")
      call _lfortran_set_item(a, 10, string)

      do i = 0, 11
         if (i / 2 == 2 * i) then
            write(str_tmp, '(I0)') i
            call _lfortran_set_item(b, i, trim(str_tmp) // "_str")
         else
            write(string, '(I0)') i
            string = trim(string) // "_str"
            call _lfortran_set_item(b, i, string)
         end if
      end do

      do i = 0, 9
         if (_lfortran_get_item(b, i) /= _lfortran_get_item(a, i)) error stop
      end do
   end subroutine


   subroutine test_list_02()
      type(_lfortran_list(character(len=:))) :: x, y
      integer :: i
      character(len=:), allocatable :: str_tmp, s

      x = _lfortran_list_constant("1", "2")

      do i = 0, 49
         write(str_tmp, '(I0)') i
         call _lfortran_list_append(x, "str_" // trim(str_tmp))
      end do

      write(str_tmp, '(A,A)') "str_", _lfortran_get_item(x, 0)
      call _lfortran_set_item(x, 0, trim(str_tmp))
      write(str_tmp, '(A,A)') "str_", _lfortran_get_item(x, 1)
      call _lfortran_set_item(x, 1, trim(str_tmp))

      do i = 0, _lfortran_len(x) - 1
         call _lfortran_list_append(y, _lfortran_get_item(x, i))
      end do

      do i = 0, _lfortran_len(y) - 1
         if (_lfortran_get_item(x, i) /= _lfortran_get_item(y, i)) error stop
      end do
   end subroutine


   subroutine tests()
      call test_list_01()
      call test_list_02()
   end subroutine

end module

program test_list_str
   use list_str_tests
   implicit none

   call tests()
end program
