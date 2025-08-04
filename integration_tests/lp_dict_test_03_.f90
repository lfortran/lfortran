module dict_test_03_mod
   implicit none
contains

   function power2(i, mod_) result(res)
      integer(8), intent(in) :: i, mod_
      integer(8) :: p1, p2, px, py, res

      if (i == 0 .or. i == 1) then
         res = 2 ** i
         return
      end if

      p1 = i / 2
      p2 = i - p1
      px = mod(power2(p1, mod_), mod_)
      py = mod(power2(p2, mod_), mod_)
      res = mod(px * py, mod_)
   end function

   function generate_key(i) result(key)
      integer, intent(in) :: i
      character(len=:), allocatable :: key
      _lfortran_dict(integer, character(len=:)) :: i2d
      integer :: mod_, key_digits, digit
      i2d = _lfortran_dict_constant(0, "a", 1, "b", 2, "c", 3, "d", 4, "e", 5, "f", 6, "g", 7, "h", 8, "i", 9, "j")
      mod_ = int(99997, kind=8)
      key_digits = int(power2(int(i, kind=8), int(mod_, kind=8)))
      key = ""

      do while (key_digits > 0)
         digit = mod(key_digits, 10)
         key = key // _lfortran_get_item(i2d, digit)
         key_digits = key_digits / 10
      end do
   end function

   subroutine test_dict()
      type(_lfortran_dict(character(len=:), integer(8))) :: number2cpi
      character(len=:),allocatable :: key
      integer :: i, size, cd, size1
      integer(8) :: val

      cd = 1
      size = cd * 150

      do i = 1000, 1000 + size - 1
         key = generate_key(i)
         val = power2(int(i, kind=8), int(99997, kind=8))
         call _lfortran_set_item(number2cpi, key, val)
      end do

      size1 = _lfortran_len(number2cpi)

      do i = 1000, 1000 + size/2 - 1
         key = generate_key(i)

         val = power2(int(i, kind=8), int(99997, kind=8))
         if (_lfortran_pop(number2cpi, key) /= val) error stop
         size1 = size1 - 1
         if (_lfortran_len(number2cpi) /= size1) error stop
      end do

      do i = 1000, 1000 + size/2 - 1
         key = generate_key(i)
         val = -power2(int(i, kind=8), int(99997, kind=8))
         call _lfortran_set_item(number2cpi, key, val)
      end do

      do i = 1000, 1000 + size/2 - 1
         key = generate_key(i)
         val = -power2(int(i, kind=8), int(99997, kind=8))
         if (_lfortran_get_item(number2cpi, key) /= val) error stop
      end do

      do i = 1000, 1000 + size - 1
         key = generate_key(i)
         val = -power2(int(i, kind=8), int(99997, kind=8))
         call _lfortran_set_item(number2cpi, key, val)
      end do

      do i = 1000, 1000 + size - 1
         key = generate_key(i)
         val = -power2(int(i, kind=8), int(99997, kind=8))
         if (_lfortran_get_item(number2cpi, key) /= val) error stop
      end do

      do i = 1000 + size/4, 1000 + size/2 - 1
         key = generate_key(i)
         print *, key, _lfortran_get_item(number2cpi, key)
      end do
   end subroutine

end module


program test_dict_main
   use dict_test_03_mod
   call test_dict()
end program
