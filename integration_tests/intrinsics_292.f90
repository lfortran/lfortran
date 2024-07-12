program intrinsics_292
   implicit none
   integer :: narg
   narg = command_argument_count()
   print *, "narg: ", narg
   if ( narg /= 0 ) error stop
end program intrinsics_292

