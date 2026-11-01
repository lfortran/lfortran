module bindc_cfi_alloc_copyback_05_mod
    implicit none
    interface
        subroutine c_alloc(d) bind(c)
            real(8), allocatable, intent(inout) :: d(:)
        end subroutine
    end interface
    type :: t
        real(8), allocatable :: v(:)
    end type
contains
    subroutine wrap(v)
        real(8), allocatable, intent(inout) :: v(:)
        call c_alloc(v)
    end subroutine
end module

program bindc_cfi_alloc_copyback_05
    use bindc_cfi_alloc_copyback_05_mod
    implicit none
    type(t) :: s(2)
    type(t), allocatable :: u(:)

    call wrap(s(2)%v)
    if (.not. allocated(s(2)%v)) error stop 1
    if (size(s(2)%v) /= 3) error stop 2
    if (any(s(2)%v /= [10, 20, 30])) error stop 3
    if (allocated(s(1)%v)) error stop 4

    allocate(u(3))
    call wrap(u(3)%v)
    if (size(u(3)%v) /= 3) error stop 5
    if (u(3)%v(2) /= 20) error stop 6
    print *, "ok"
end program
