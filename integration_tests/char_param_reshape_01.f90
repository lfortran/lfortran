module char_param_mod
   implicit none
   character(7), parameter :: ch_arr(1:2,1:3) = reshape([ &
      'a      ', '1      ', &
      'b      ', '2      ', &
      'c      ', '3      '  &
      ], [2,3])
contains
   function test(i)
      integer, intent(in) :: i
      character(:), allocatable :: test
      test = ch_arr(1, i)
   end function test
end module

program char_param_reshape_01
   use char_param_mod
   implicit none
   if (trim(test(1)) /= "a") error stop
   if (trim(test(2)) /= "b") error stop
   if (trim(test(3)) /= "c") error stop
end program char_param_reshape_01
