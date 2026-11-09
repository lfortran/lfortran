! An INTENT(IN) pointer dummy is distinguishable from an allocatable dummy
! (generic_distinguishable). A bare allocatable actual must resolve to the
! allocatable specific even when the pointer specific is listed first.
! A nonpointer target still resolves to the INTENT(IN) pointer specific.
! GFortran rejects this generic (F2018 15.4.3.4.5: a pointer with INTENT(IN)
! does not distinguish itself from an allocatable). LFortran keeps the
! procedures distinguishable and has to select the allocatable specific.
module generic_interface_distinguishable_05_m
    implicit none
    integer :: which = 0
    interface g_ptr_in_first
        module procedure ptr_in, alloc_s
    end interface
    interface g_alloc_first
        module procedure alloc_s, ptr_in
    end interface
contains
    subroutine ptr_in(x)
        real, pointer, intent(in) :: x(:)
        which = 2
        if (.not. associated(x)) error stop 20
        if (size(x) /= 2) error stop 21
        if (abs(x(1) - 3.0) > 1.0e-6) error stop 22
    end subroutine
    subroutine alloc_s(x)
        real, allocatable, intent(inout) :: x(:)
        which = 1
        if (.not. allocated(x)) error stop 30
        if (size(x) /= 2) error stop 31
        if (abs(x(1) - 1.0) > 1.0e-6) error stop 32
    end subroutine
end module

program generic_interface_distinguishable_05
    use generic_interface_distinguishable_05_m
    implicit none
    real, allocatable :: xa(:)
    real, allocatable, target :: xt(:)
    real, pointer :: xp(:)
    real, target :: t(2)
    allocate(xa(2), xt(2), xp(2))
    xa = 1.0
    xt = 1.0
    xp = 3.0
    t = 3.0
    call g_ptr_in_first(xa)
    if (which /= 1) error stop 1
    call g_alloc_first(xa)
    if (which /= 1) error stop 2
    call g_ptr_in_first(xp)
    if (which /= 2) error stop 3
    call g_alloc_first(xp)
    if (which /= 2) error stop 4
    call g_ptr_in_first(t)
    if (which /= 2) error stop 5
    call g_alloc_first(t)
    if (which /= 2) error stop 6
    call g_ptr_in_first(t(1:2))
    if (which /= 2) error stop 7
    call g_ptr_in_first(xt)
    if (which /= 1) error stop 8
    call g_alloc_first(xt)
    if (which /= 1) error stop 9
    deallocate(xa, xt, xp)
    print *, "ok"
end program
