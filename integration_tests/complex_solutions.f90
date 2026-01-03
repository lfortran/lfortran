program complex_solutions

   ! print some complex solutions to the equation: 1**x = rhs.
   
   use, intrinsic :: iso_fortran_env, only: wp=>real64
   implicit none
   complex(wp) :: z1, arg, x
   real(wp), parameter :: rhs = 4.0_wp, twopi = 8 * atan(1.0_wp), lnrhs = log(rhs), tol = 1.0e-12_wp
   integer :: k
   character(*), parameter :: cfmth='("Complex solutions to 1**x=",g0.4/a2,*(4x,a11,5x))', &
        &  cfmt='(i2,*(" (",es0.2,",",es0.2,")":))'

   write(*,cfmth) rhs, 'k', 'arg=i2Pik', 'exp(arg)', 'x', 'exp(arg)**x', 'exp(arg*x)'

   do k = 1, 10
      arg = cmplx(0.0_wp, twopi * k, kind=wp)
      z1  = exp(arg)
      x   = cmplx(0.0_wp, -lnrhs/(twopi*k), kind=wp)

      ! Check for correctness:
      if (abs(real(exp(arg*x)) - rhs) > tol .or. abs(aimag(exp(arg*x))) > tol) then
         print *, "Integration test failed for k=", k
         error stop 1
      end if

      write(*,cfmt) k, arg, z1, x, z1**x, exp(arg*x)
   end do

end program complex_solutions
