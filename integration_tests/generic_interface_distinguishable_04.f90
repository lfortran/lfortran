! Generic resolution must use ALLOCATABLE versus POINTER when choosing a
! specific (issue 17023). A pointer actual must not resolve to the
! allocatable specific, in either listing order.
module generic_interface_distinguishable_04_m
    implicit none
    integer :: which = 0
    interface g_alloc_first
        module procedure ap_alloc, ap_ptr
    end interface
    interface g_ptr_first
        module procedure ap_ptr, ap_alloc
    end interface
contains
    subroutine ap_alloc(x)
        real, allocatable, intent(inout) :: x(:)
        which = 1
    end subroutine
    subroutine ap_ptr(x)
        real, pointer, intent(inout) :: x(:)
        which = 2
    end subroutine
end module

program generic_interface_distinguishable_04
    use generic_interface_distinguishable_04_m
    implicit none
    real, allocatable :: xa(:)
    real, pointer :: xp(:)
    allocate(xa(2), xp(2))
    call g_alloc_first(xa)
    if (which /= 1) error stop 1
    call g_alloc_first(xp)
    if (which /= 2) error stop 2
    call g_ptr_first(xa)
    if (which /= 1) error stop 3
    call g_ptr_first(xp)
    if (which /= 2) error stop 4
    deallocate(xa, xp)
    print *, "ok"
end program
