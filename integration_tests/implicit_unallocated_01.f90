! An unallocated allocatable may be passed to an implicit-interface
! procedure. The call must not be rejected as an unallocated argument.
program implicit_unallocated_01
    implicit none
    real, allocatable :: a(:)
    external show
    call show(a, allocated(a))
    allocate(a(2))
    a = 1.0
    call show(a, allocated(a))
end program
