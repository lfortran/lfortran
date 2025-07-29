program complex_solutions
   use, intrinsic :: iso_fortran_env, only: wp => real64
   implicit none
   complex(wp) :: arg, x, val
   real(wp), parameter :: rhs = 4.0_wp, twopi = 8 * atan(1.0_wp), lnrhs = log(rhs), tol = 1.0e-12_wp
   integer :: k

   do k = 1, 10
      arg = cmplx(0.0_wp, twopi * k, kind=wp)
      x   = cmplx(0.0_wp, -lnrhs/(twopi*k), kind=wp)
      val = exp(arg*x)

      if (abs(real(val) - rhs) > tol .or. abs(aimag(val)) > tol) then
         error stop "Integration test failed for k=" // trim(adjustl(itoa(k)))
      end if
   end do

contains
   pure function itoa(i) result(str)
      integer, intent(in) :: i
      character(len=32) :: str
      write(str,'(I0)') i
   end function itoa
end program complex_solutions
