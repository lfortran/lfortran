module dict_test_07_mod
   implicit none
contains

   function fill_smalltocapital() result(smalltocaps)
      type(_lfortran_dict(character(len=:), character(len=:))) :: smalltocaps
      smalltocaps = _lfortran_dict_constant( &
         "a", "A", "b", "B", "c", "C", "d", "D", "e", "E", &
         "f", "F", "g", "G", "h", "H", "i", "I", "j", "J", &
         "k", "K", "l", "L", "m", "M", "n", "N", "o", "O", &
         "p", "P", "q", "Q", "r", "R", "s", "S", "t", "T", &
         "u", "U", "v", "V", "w", "W", "x", "X", "y", "Y", "z", "Z")
   end function

   subroutine test_dict()
      type(_lfortran_dict(character(len=:), character(len=:))) :: smalltocaps
      integer :: i
      character(len=1) :: key, expected

      smalltocaps = fill_smalltocapital()

      if (_lfortran_len(smalltocaps) /= 26) error stop

      do i = 97, 97 + 25
         key = achar(i)
         expected = achar(i - 32)
         if (_lfortran_get_item(smalltocaps, key) /= expected) error stop
      end do
   end subroutine

end module

program test_dict_07
   use dict_test_07_mod
   implicit none

   call test_dict()
end program
