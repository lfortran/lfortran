program format_23
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=dp) :: t
real(kind=dp) :: my_dp_eps

   ! starting with a value of 1, keep dividing the value
   ! by 2 until no change is detected. Note that with
   ! infinite precision this would be an infinite loop.
   ! 1/2 of a non-zero number is always smaller than the number, right?
   my_dp_eps = 1.0d0
   INFINITE: do
               my_dp_eps = my_dp_eps/2.0d0
               t = 1.0d0 + my_dp_eps
               if (t <= 1.0d0) exit
   enddo INFINITE
   my_dp_eps = 2.0d0*my_dp_eps
   print *, my_dp_eps
   print *, 'weird coincidence, that is the same as ',epsilon(0.0d0)
   print *, 'congratulations! You calculated the epsilon value of a machine the hard way!'
   print '(b64.64)',my_dp_eps

end program format_23
