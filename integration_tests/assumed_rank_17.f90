! A list-directed READ of a *whole* assumed-rank dummy argument used to pass the
! address of the array descriptor's data-pointer field to the runtime instead of
! the data pointer itself, so the value read was stored over the descriptor and
! the actual argument was never assigned.
! On a typed-pointer LLVM the same bad lowering was rejected by the module
! verifier with "Call parameter type does not match function signature!".
! Affects every rank, not just rank 0.
! See https://github.com/lfortran/lfortran/issues/13092
module assumed_rank_17_mod
   implicit none
contains

   subroutine read_any_rank(iunit, var)
      integer, intent(in) :: iunit
      real(8), intent(inout) :: var(..)
      select rank (var)
      rank (0)
         read (iunit, *) var
      rank (1)
         read (iunit, *) var
      rank default
         error stop "unexpected rank"
      end select
   end subroutine read_any_rank

   subroutine read_int_rank0(iunit, var)
      integer, intent(in) :: iunit
      integer, intent(inout) :: var(..)
      select rank (var)
      rank (0)
         read (iunit, *) var
      rank default
         error stop "unexpected rank"
      end select
   end subroutine read_int_rank0

end module assumed_rank_17_mod

program assumed_rank_17
   use assumed_rank_17_mod
   implicit none

   real(8) :: s
   real(8) :: a(3)
   integer :: n
   integer :: u
   integer :: i

   open (newunit=u, file="assumed_rank_17_data.txt", status="replace", &
         action="readwrite")
   write (u, *) 1.5d0
   write (u, *) 10.0d0, 20.0d0, 30.0d0
   write (u, *) 7
   rewind (u)

   s = -1.0d0
   call read_any_rank(u, s)
   print *, s
   if (abs(s - 1.5d0) > 1.0d-12) error stop "rank 0 read did not assign"

   a = -1.0d0
   call read_any_rank(u, a)
   print *, a
   do i = 1, 3
      if (abs(a(i) - 10.0d0*i) > 1.0d-12) error stop "rank 1 read did not assign"
   end do

   n = -1
   call read_int_rank0(u, n)
   print *, n
   if (n /= 7) error stop "integer rank 0 read did not assign"

   close (u, status="delete")

end program assumed_rank_17
