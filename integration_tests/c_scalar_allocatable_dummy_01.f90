module c_scalar_allocatable_dummy_01_m
    implicit none
    type :: item
        integer, allocatable :: value
    end type
    type :: plain
        integer :: value
    end type
contains
    subroutine create(x)
        integer, allocatable, intent(out) :: x
        if (allocated(x)) error stop 1
        allocate(x)
        x = 7
        if (x /= 7) error stop 2
        if (x + 1 /= 8) error stop 22
    end subroutine

    subroutine replace(x)
        integer, allocatable, intent(inout) :: x
        if (.not. allocated(x)) error stop 3
        if (x /= 7) error stop 4
        deallocate(x)
        if (allocated(x)) error stop 5
        allocate(x)
        x = 9
    end subroutine

    subroutine inspect(x)
        integer, allocatable, intent(in) :: x
        if (.not. allocated(x)) error stop 6
        if (x /= 9) error stop 7
    end subroutine

    subroutine replace_unspecified(x)
        integer, allocatable :: x
        deallocate(x)
        allocate(x)
        x = 11
    end subroutine

    subroutine bump(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine

    subroutine create_plain(x)
        type(plain), allocatable, intent(out) :: x
        allocate(x)
        x%value = 13
    end subroutine

    subroutine inspect_plain(x)
        type(plain), allocatable, intent(in) :: x
        if (x%value /= 13) error stop 17
    end subroutine

    subroutine captured()
        integer, allocatable :: local
        call inner()
        if (.not. allocated(local)) error stop 8
        if (local /= 7) error stop 9
        deallocate(local)
    contains
        subroutine inner()
            call create(local)
        end subroutine
    end subroutine
end module

program c_scalar_allocatable_dummy_01
    use c_scalar_allocatable_dummy_01_m
    implicit none
    integer, allocatable :: x, copy
    type(item) :: obj
    type(plain), allocatable :: allocated_obj

    allocate(x)
    x = 42
    call create(x)
    if (.not. allocated(x)) error stop 10
    if (x /= 7) error stop 11
    allocate(copy)
    copy = x
    if (copy /= 7) error stop 24
    deallocate(copy)
    call replace(x)
    if (.not. allocated(x)) error stop 12
    if (x /= 9) error stop 13
    call inspect(x)
    call replace_unspecified(x)
    if (.not. allocated(x)) error stop 18
    if (x /= 11) error stop 19
    call bump(x)
    if (x /= 12) error stop 23
    deallocate(x)
    if (allocated(x)) error stop 14

    call create(obj%value)
    if (.not. allocated(obj%value)) error stop 15
    if (obj%value /= 7) error stop 16
    deallocate(obj%value)
    call create_plain(allocated_obj)
    if (.not. allocated(allocated_obj)) error stop 20
    if (allocated_obj%value /= 13) error stop 21
    call inspect_plain(allocated_obj)
    deallocate(allocated_obj)
    call captured()
end program
