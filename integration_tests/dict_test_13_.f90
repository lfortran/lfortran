module dict_test_13_mod
   implicit none
   type(_lfortran_dict(character(len=:), integer(4))) :: I4C

contains

   subroutine init_I4C()
      I4C = _lfortran_dict_constant( &
         "0", 0, "1", 1, "2", 2, "3", 3, &
         "4", 4, "5", 5, "6", 6, "7", 7, &
         "8", 8, "9", 9, &
         "a", 10, "b", 11, "c", 12, "d", 13, &
         "A", 10, "B", 11, "C", 12, "D", 13, &
         "e", 14, "f", 15, &
         "E", 14, "F", 15)
   end subroutine

   function cnvi(s, base) result(result)
      character(len=*), intent(in) :: s
      integer(4), intent(in) :: base
      integer(4) :: result
      integer :: i, len_s
      character(len=1) :: c
      integer(4) :: pow_, incr

      if (base /= 10 .and. base /= 8 .and. base /= 16 .and. base /= 2) error stop
      call init_I4C()

      len_s = len(s)
      result = 0
      pow_ = base ** (len_s - 1)

      do i = 1, len_s
         c = s(i:i)
         incr = pow_ * _lfortran_get_item(I4C, c)
         result = result + incr
         pow_ = pow_ / base
      end do
   end function

   subroutine test_dict()
      integer(4) :: result

      result = cnvi("0b0", 2)
      print *, result
      if (result /= 22) error stop

      result = cnvi("0b1", 2)
      print *, result
      if (result /= 23) error stop

      result = cnvi("0b10", 2)
      print *, result
      if (result /= 46) error stop

      result = cnvi("0b11", 2)
      print *, result
      if (result /= 47) error stop

      result = cnvi("0b11110100111", 2)
      print *, result
      if (result /= 24487) error stop

      result = cnvi("0b7a7", 16)
      print *, result
      if (result /= 47015) error stop
   end subroutine

end module

program test_dict_13
   use dict_test_13_mod
   implicit none

   call test_dict()
end program
