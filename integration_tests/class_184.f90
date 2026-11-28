module class_184_mod
    implicit none
    type :: slot
        integer :: id
        real :: w
    end type
contains
    subroutine mark(s)
        type(slot) :: s(:)
        if (size(s) /= 5) error stop 1
        if (lbound(s, 1) /= 1) error stop 2
        if (s(2)%id /= 20) error stop 3
        s(4)%id = -s(4)%id
    end subroutine

    subroutine mark_target(s)
        type(slot), target :: s(:)
        type(slot), pointer :: p
        if (size(s) /= 5) error stop 4
        p => s(3)
        p%id = -p%id
    end subroutine

    subroutine mark_optional(s, t)
        type(slot), intent(inout), optional :: s(:)
        type(slot), intent(in), optional :: t(:)
        if (present(t)) error stop 5
        if (.not. present(s)) error stop 6
        if (size(s) /= 5) error stop 7
        s(2)%id = -s(2)%id
    end subroutine

    integer function total(s)
        type(slot) :: s(:)
        integer :: i
        total = 0
        do i = 1, size(s)
            total = total + s(i)%id
        end do
    end function

    integer function total_in(s)
        type(slot), intent(in) :: s(:)
        integer :: i
        total_in = 0
        do i = 1, size(s)
            total_in = total_in + s(i)%id
        end do
    end function

    subroutine mark2(s)
        type(slot) :: s(:, :)
        if (size(s, 1) /= 2) error stop 8
        if (size(s, 2) /= 3) error stop 9
        if (s(1, 2)%id /= 3) error stop 10
        s(2, 3)%id = 99
    end subroutine

    subroutine check_ids(s, expected)
        type(slot) :: s(:)
        integer, intent(in) :: expected(:)
        if (size(s) /= size(expected)) error stop 26
        if (any(s%id /= expected)) error stop 27
        s(1)%id = -s(1)%id
    end subroutine

    subroutine via_dummy(c)
        class(slot), intent(inout) :: c(:)
        call mark(c)
    end subroutine

    subroutine via_allocatable_dummy(c)
        class(slot), allocatable, intent(inout) :: c(:)
        call mark(c)
    end subroutine

    subroutine via_pointer_dummy(c)
        class(slot), pointer, intent(inout) :: c(:)
        call mark(c)
    end subroutine
end module

program class_184
    use class_184_mod
    implicit none
    class(slot), allocatable :: rack(:)
    class(slot), allocatable :: grid(:, :)
    class(slot), pointer :: shelf(:)
    type(slot), target :: bins(3)
    integer :: i, j

    allocate(rack(5))
    rack%id = [10, 20, 30, 40, 50]
    rack%w = 1.5

    call mark(rack)
    if (any(rack%id /= [10, 20, 30, -40, 50])) error stop 11

    call mark_target(rack)
    if (any(rack%id /= [10, 20, -30, -40, 50])) error stop 12

    call mark_optional(rack)
    if (any(rack%id /= [10, -20, -30, -40, 50])) error stop 13

    if (total(rack) /= -30) error stop 14
    if (any(rack%w /= 1.5)) error stop 15

    rack%id = [10, 20, 30, 40, 50]
    call via_dummy(rack)
    if (any(rack%id /= [10, 20, 30, -40, 50])) error stop 16
    call via_allocatable_dummy(rack)
    if (any(rack%id /= [10, 20, 30, 40, 50])) error stop 17
    call local_mark(rack)
    if (any(rack%id /= [-10, 20, 30, 40, 50])) error stop 18

    rack%id = [10, 20, 30, 40, 50]
    call check_ids(rack(2:4), [20, 30, 40])
    if (any(rack%id /= [10, -20, 30, 40, 50])) error stop 28
    call check_ids(rack(5:1:-2), [50, 30, 10])
    if (any(rack%id /= [10, -20, 30, 40, -50])) error stop 29

    allocate(grid(2, 3))
    do j = 1, 3
        do i = 1, 2
            grid(i, j)%id = i + 2*(j - 1)
        end do
    end do
    call mark2(grid)
    if (grid(2, 3)%id /= 99) error stop 19
    if (grid(1, 3)%id /= 5) error stop 20

    allocate(shelf(5))
    shelf%id = [10, 20, 30, 40, 50]
    call mark(shelf)
    if (any(shelf%id /= [10, 20, 30, -40, 50])) error stop 21
    call via_pointer_dummy(shelf)
    if (any(shelf%id /= [10, 20, 30, 40, 50])) error stop 22
    call mark_target(shelf)
    if (any(shelf%id /= [10, 20, -30, 40, 50])) error stop 23
    if (total(shelf) /= 90) error stop 24
    if (total_in(shelf) /= 90) error stop 30
    deallocate(shelf)

    bins%id = [3, 4, 5]
    shelf => bins
    if (total(shelf) /= 12) error stop 31
    if (total_in(shelf) /= 12) error stop 32
    call check_ids(shelf, [3, 4, 5])
    if (any(bins%id /= [-3, 4, 5])) error stop 33

    print *, "OK"
contains
    subroutine local_mark(s)
        type(slot) :: s(:)
        if (size(s) /= 5) error stop 25
        s(1)%id = -s(1)%id
    end subroutine
end program
