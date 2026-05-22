module associate_53_m
   use iso_c_binding, only: C_FLOAT
   implicit none
   type :: tin
      real(C_FLOAT), pointer :: pxVel(:) => null()
   end type
contains
   subroutine work(ValAry, u, lo, hi)
      real(8), intent(in)      :: ValAry(:)
      type(tin), intent(inout) :: u
      integer, intent(in)      :: lo, hi
      associate (VarVals => ValAry(lo:hi))
         u%pxVel(1:hi-lo+1) = VarVals
      end associate
      u%pxVel(1:hi-lo+1) = ValAry(lo:hi)
   end subroutine
end module

program associate_53
   use iso_c_binding, only: C_FLOAT
   use associate_53_m, only: tin, work
   implicit none

   real(8) :: ValAry(5)
   real(C_FLOAT), target :: storage(5)
   type(tin) :: u
   integer :: i

   do i = 1, 5
      ValAry(i) = real(i, kind=8)
   end do

   u%pxVel => storage
   call work(ValAry, u, 2, 4)

   if (any(abs(u%pxVel(1:3) - real(ValAry(2:4), kind=C_FLOAT)) > 0.0_C_FLOAT)) then
      error stop
   end if

   print *, "successful"
end program associate_53
