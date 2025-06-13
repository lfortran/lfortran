module list_test_03_mod
   implicit none
contains

   function test_list_01(n) result(sum)
      integer, intent(in) :: n
      integer :: sum, i
      type(_lfortran_list(integer)) :: a

      sum = 0
      do i = 0, n-1
         call _lfortran_list_append(a, i)
      end do

      do i = 0, n-1
         sum = sum + _lfortran_get_item(a, i)
      end do
   end function

   function test_list_insert_02(x, n) result(out)
      type(_lfortran_list(integer)), intent(inout) :: x
      integer, intent(in) :: n
      integer :: i, imod
      type(_lfortran_list(integer)) :: out

      out = x

      do i = 0, n-1
         imod = mod(i, 3)
         if (imod == 0) then
            call _lfortran_list_insert(out, 0, i + n)
         else if (imod == 1) then
            call _lfortran_list_insert(out, _lfortran_len(out), i + n + 1)
         else if (imod == 2) then
            call _lfortran_list_insert(out, _lfortran_len(out)/2, i + n + 2)
         end if
      end do
   end function

   function test_list_02(n) result(acc)
      integer, intent(in) :: n
      integer :: acc, i
      type(_lfortran_list(integer)) :: x

      x = _lfortran_list_constant(50, 1)
      acc = 0

      x = test_list_insert_02(x, n)

      do i = 0, n-1
         acc = acc + _lfortran_get_item(x, i)
      end do
   end function

   subroutine test_list_02_string()
      type(_lfortran_list(character(len=:))) :: x, y
      character(len=:), allocatable :: string
      character(len=:) :: buffer
      integer :: i, imod

      do i = 0, 49
         write(buffer, '(I0)') i + mod(i, 3)
         string = "xd_" // trim(buffer)
         call _lfortran_list_append(y, string)
      end do

      do i = 0, 49
         imod = mod(i, 3)
         write(buffer, '(I0)') i + imod
         string = "xd_" // trim(buffer)
         call _lfortran_list_insert(x, _lfortran_len(x), string)
      end do

      do i = 0, 49
         if (_lfortran_get_item(x, i) /= _lfortran_get_item(y, i)) error stop
      end do
   end subroutine

   subroutine verify()
      if (test_list_01(11) /= 55) error stop
      if (test_list_02(50) /= 3628) error stop
      call test_list_02_string()
   end subroutine

end module

program test_list_insert_
   use list_test_03_mod
   implicit none

   call verify()
end program
