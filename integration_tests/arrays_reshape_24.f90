! Testing for correct handling of compile-time value (if present) during `Var` node
! replacement with `m_value` for a module variable by `array_struct_temporary` pass.

module arrays_reshape_24_mod
   implicit none

   character(len=*,kind=1), parameter :: test1data(2,2) = reshape([character(len=2) :: "ab", "cd", "ef", "gh"], [2, 2])

end module

program arrays_reshape_24
   use arrays_reshape_24_mod

   print *, size(test1data, 1)
   if (size(test1data, 1) /= 2) error stop

   print *, size(test1data, 2)
   if (size(test1data, 2) /= 2) error stop

end program
