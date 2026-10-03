module interface_34_m
   implicit none
   interface take
      module procedure take_logical
   end interface take
contains
   subroutine take_logical(flag)
      logical, intent(in) :: flag
      print *, flag
   end subroutine take_logical

   subroutine probe(y)
      real, intent(in) :: y(:)
      call take(is_contiguous(y))          ! ICE here
   end subroutine probe

   subroutine probe_ok(y)
      real, intent(in) :: y(:)
      call take_logical(is_contiguous(y))  ! specific call: fine
   end subroutine probe_ok
end module interface_34_m

program interface_34
use interface_34_m
implicit none

end program interface_34