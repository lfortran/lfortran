program assumed_rank_19
   implicit none

   real(8) :: a(1) = [1.0d0]
   real(8) :: b(1)
   integer :: u

   open (newunit=u, file="assumed_rank_19_unformatted_write.dat", &
         form="unformatted", status="replace", action="readwrite")
   call write_assumed_rank(u, a)
   rewind (u)
   b = -1.0d0
   read (u) b
   print *, b
   if (any(b /= a)) error stop "assumed-rank unformatted write corrupted data"
   close (u, status="delete")

contains

   subroutine write_assumed_rank(u, x)
      integer, intent(in) :: u
      real(8), intent(in) :: x(..)

      select rank (x)
      rank (1)
         write (u) x
      rank default
         error stop "unexpected rank"
      end select
   end subroutine write_assumed_rank

end program assumed_rank_19
