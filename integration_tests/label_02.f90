! An END= label on a statement inside the same IF block as the READ (#11499).
program label_02
   implicit none
   integer :: u, i, nend

   open(newunit=u, status='scratch')
   write(u, *) 5
   rewind(u)
   nend = 0
   i = 1
   call s(u, i, nend)
   if (i /= 5) error stop
   if (nend /= 0) error stop
   call s(u, i, nend)
   if (nend /= 1) error stop
   close(u)
   print *, 'ok'

contains

   subroutine s(u, i, nend)
      integer, intent(in) :: u
      integer, intent(inout) :: i, nend
      integer :: j

      if (i > 0) then
         read(u, *, end=20) j
         i = j
         return
20       nend = nend + 1
      end if
   end subroutine
end program label_02
