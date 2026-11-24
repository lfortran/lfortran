module allocatable_scalar_assign_02_m
    implicit none
    type :: holder
        integer, allocatable :: item
    end type
contains
    subroutine write_out(x)
        integer, allocatable, intent(out) :: x
        if (allocated(x)) error stop 1
        x = 31
        if (.not. allocated(x)) error stop 2
    end subroutine

    subroutine rewrite_inout(x)
        integer, allocatable, intent(inout) :: x
        deallocate(x)
        if (allocated(x)) error stop 3
        x = 41
        if (.not. allocated(x)) error stop 4
    end subroutine
end module

program allocatable_scalar_assign_02
    use allocatable_scalar_assign_02_m
    implicit none
    integer, allocatable :: x
    real(8), allocatable :: r
    type(holder) :: h

    x = 10
    if (.not. allocated(x)) error stop 5
    if (x /= 10) error stop 6
    x = 20
    if (x /= 20) error stop 7

    h%item = x
    if (.not. allocated(h%item)) error stop 8
    if (h%item /= 20) error stop 9
    h%item = 25
    if (h%item /= 25) error stop 10

    call write_out(x)
    if (.not. allocated(x)) error stop 11
    if (x /= 31) error stop 12
    call rewrite_inout(x)
    if (x /= 41) error stop 13
    deallocate(x)
    if (allocated(x)) error stop 14
    x = 51
    if (.not. allocated(x)) error stop 15
    if (x /= 51) error stop 16

    r = 1.5d0
    if (.not. allocated(r)) error stop 17
    if (r < 1.4d0 .or. r > 1.6d0) error stop 18
    deallocate(x, r, h%item)
    if (allocated(x)) error stop 19
    if (allocated(r)) error stop 20
    if (allocated(h%item)) error stop 21
end program
