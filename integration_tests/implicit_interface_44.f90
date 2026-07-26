program implicit_interface_44
   implicit none
   external implicit_interface_44_ode
   integer, parameter :: neqn = 4
   integer :: iflag, iwork(5)
   real(8) :: relerr, abserr, t, tout
   real(8) :: y(neqn), work(100+21*neqn)

   t = 1.0_8
   y(1) = 2.0_8
   tout = 5.0_8

   call implicit_interface_44_ode(func,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)
   if (t /= 5.0_8) then
       error stop "Error in implicit_interface_44"
   end if
contains
   subroutine func(x,y,yp)
      real(8) :: x
      real(8) :: y(4)
      real(8) :: yp(4)
      yp(1) = x + y(1)
   end subroutine func
end program implicit_interface_44
