program assumed_rank_19
   implicit none

   real(8) :: a(1) = [1.0d0]
   real(8) :: b(1)
   real(8) :: strided_source(6), strided_result(3), strided_expected(3)
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

   strided_source = [10.0d0, 20.0d0, 30.0d0, 40.0d0, 50.0d0, 60.0d0]
   strided_expected = [10.0d0, 30.0d0, 50.0d0]
   open (newunit=u, file="assumed_rank_19_unformatted_write_strided.dat", &
         form="unformatted", status="replace", action="readwrite")
   call write_assumed_rank(u, strided_source(1:6:2))
   rewind (u)
   strided_result = -1.0d0
   read (u) strided_result
   print *, strided_result
   if (any(strided_result /= strided_expected)) then
      error stop "strided assumed-rank unformatted write corrupted data"
   end if
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
