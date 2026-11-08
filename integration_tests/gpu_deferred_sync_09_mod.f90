! A library compiled for the host only (HOST_STATIC_LIBRARY) in the default
! mode: its object is empty, and the unit with the main program compiles
! this subroutine.
module gpu_deferred_sync_09_mod
implicit none
contains
subroutine add_one(n, a)
    integer, intent(in) :: n
    integer, intent(inout) :: a(n)
    integer :: i
    do i = 1, n
        a(i) = a(i) + 1
    end do
end subroutine
end module
