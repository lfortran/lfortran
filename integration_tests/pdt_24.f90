module pdt_reset_ext_m
    implicit none
    type :: base(k)
        integer, kind :: k = 4
        integer(k) :: typed
        integer :: parent_default = 17
        integer, allocatable :: parent_data(:)
    end type
    type, extends(base) :: child
        integer :: child_default = 42
        integer, allocatable :: child_data(:)
    end type
contains
    subroutine reset_default(x)
        class(child), intent(out) :: x
    end subroutine

    subroutine reset_wide(x)
        class(child(8)), intent(out) :: x
    end subroutine
end module

program pdt_reset_ext
    use pdt_reset_ext_m
    implicit none
    type(child) :: a
    type(child(8)) :: b
    if (a%k /= 4 .or. b%k /= 8) error stop 1
    if (storage_size(a%typed) /= 32 .or. storage_size(b%typed) /= 64) error stop 2
    a%parent_default = 99
    a%child_default = 99
    b%parent_default = 99
    b%child_default = 99
    allocate(a%parent_data(1), a%child_data(1))
    allocate(b%parent_data(1), b%child_data(1))
    call reset_default(a)
    call reset_wide(b)
    if (a%parent_default /= 17 .or. a%child_default /= 42) error stop 3
    if (b%parent_default /= 17 .or. b%child_default /= 42) error stop 4
    if (allocated(a%parent_data) .or. allocated(a%child_data)) error stop 5
    if (allocated(b%parent_data) .or. allocated(b%child_data)) error stop 6
    if (a%k /= 4 .or. b%k /= 8) error stop 7
end program
