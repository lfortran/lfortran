module update

contains
   subroutine updateres(b, delta, rescon)
      implicit none

      real, intent(in) :: b(:)
      real, intent(in) :: delta
      real, intent(inout) :: rescon(:)

      real :: ax(size(b))
      integer :: idx(size(b))    ! Comment out to avoid abort
      logical :: mask(size(b))

      ax = 1.0

      mask = (abs(rescon) < delta)

      where (mask)
         rescon = max(b - ax, 0.0)
      end where

   end subroutine updateres
end module update

program where_16
   use update
   implicit none

   real :: delta = 2.33

   real :: b(2) = [5.4, 3.6]
   real :: rescon(2) = [2.3, 8.6]

   call updateres(b, delta, rescon)
   print *, rescon
   if( any(abs(rescon - [4.4, 8.6]) > 1e-6) ) error stop

end program where_16
