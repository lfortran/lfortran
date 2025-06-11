module list_test_09_mod
   implicit none
   real :: eps = 1e-6
contains

   subroutine test_list_concat()
      type(_lfortran_list(integer)) :: x, y, z
      type(_lfortran_list(character(:))) :: c, d
      integer :: i

      z = _lfortran_concat(x, y)
      if (_lfortran_len(z) /= 0) error stop

      x = _lfortran_list_constant(1, 2, 3)
      z = _lfortran_concat(x, y)
      do i = 1, 3
         if (_lfortran_get_item(z, i - 1) /= i) error stop
      end do

      call _lfortran_clear(x)
      y = _lfortran_list_constant(6, 7, 8)
      z = _lfortran_concat(x, y)
      do i = 1, 3
         if (_lfortran_get_item(z, i - 1) /= i + 5) error stop
      end do

      x = _lfortran_list_constant(1, 2, 3, 4, 5)
      z = _lfortran_concat(x, y)
      do i = 1, 8
         if (_lfortran_get_item(z, i - 1) /= i) error stop
      end do

      call _lfortran_clear(x)
      call _lfortran_clear(y)
      do i = 9, 50
         call _lfortran_list_append(x, i)
      end do
      do i = 51, 100
         call _lfortran_list_append(y, i)
      end do

      z = _lfortran_concat(_lfortran_concat(z, x), y)

      call _lfortran_set_item(x, 0, 0)
      call _lfortran_set_item(x, 1, 0)

      call _lfortran_clear(y)

      do i = 1, 99
         if (_lfortran_get_item(z, i - 1) /= i) error stop
      end do

      c = _lfortran_list_constant('a', 'b')
      d = _lfortran_list_constant('c', 'd', 'e')
      c = _lfortran_concat(c, d)

      if (_lfortran_len(c) /= 5) error stop
      do i = 0, 4
         if (iachar(_lfortran_get_item(c, i)) - iachar('a') /= i) error stop
      end do
   end subroutine

end module

program test_list_concat_
   use list_test_09_mod
   implicit none

   call test_list_concat()
end program
