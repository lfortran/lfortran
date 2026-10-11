module class_162_mod
    implicit none

    type :: item_t
        integer :: nfld
    end type

    type, extends(item_t) :: item_ext_t
        integer :: extra
    end type

    type :: bounds_t
        integer :: first
        integer :: idx(2)
    end type

    type :: holder_t
        class(item_t), allocatable :: items(:)
    end type

contains

    integer function get_first(bounds, items) result(r)
        type(bounds_t), intent(in) :: bounds
        class(item_t), intent(in) :: items(:)
        r = items(bounds%first)%nfld
    end function

    subroutine set_first(bounds, items, val)
        type(bounds_t), intent(in) :: bounds
        class(item_t), intent(inout) :: items(:)
        integer, intent(in) :: val
        items(bounds%first)%nfld = val
    end subroutine

    integer function get_2d(bounds, items) result(r)
        type(bounds_t), intent(in) :: bounds
        class(item_t), intent(in) :: items(:, :)
        r = items(bounds%idx(1), bounds%idx(2))%nfld
    end function

    integer function get_explicit(bounds, items) result(r)
        type(bounds_t), intent(in) :: bounds
        class(item_t), intent(in) :: items(bounds%first)
        r = items(1)%nfld + items(bounds%first)%nfld
    end function

    integer function get_holder(bounds, h) result(r)
        type(bounds_t), intent(in) :: bounds
        type(holder_t), intent(in) :: h
        r = h%items(bounds%first)%nfld
    end function

end module

program class_162
    use class_162_mod
    implicit none
    type(item_t) :: items(3)
    type(item_ext_t) :: ext_items(4)
    type(item_t) :: grid(2, 3)
    type(bounds_t) :: b
    type(holder_t) :: h
    integer :: i, j

    items(1)%nfld = 10
    items(2)%nfld = 20
    items(3)%nfld = 30
    b%first = 2
    b%idx = [2, 3]

    if (get_first(b, items) /= 20) error stop 1
    call set_first(b, items, 25)
    if (items(2)%nfld /= 25) error stop 2

    do i = 1, 4
        ext_items(i)%nfld = 100 + i
        ext_items(i)%extra = -i
    end do
    b%first = 3
    if (get_first(b, ext_items) /= 103) error stop 3
    if (get_explicit(b, ext_items) /= 204) error stop 8
    call set_first(b, ext_items, 7)
    if (ext_items(3)%nfld /= 7) error stop 4
    if (ext_items(3)%extra /= -3) error stop 5

    do j = 1, 3
        do i = 1, 2
            grid(i, j)%nfld = 10*i + j
        end do
    end do
    if (get_2d(b, grid) /= 23) error stop 6

    allocate(item_ext_t :: h%items(3))
    h%items(1)%nfld = 1
    h%items(2)%nfld = 2
    h%items(3)%nfld = 3
    b%first = 3
    if (get_holder(b, h) /= 3) error stop 7

    print *, "ok"
end program
