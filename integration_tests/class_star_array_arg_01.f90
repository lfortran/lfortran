! A class(t) array actual associated with a class(*) array dummy keeps its
! dynamic type and its element data.
module class_star_array_arg_01_m
    implicit none
    type :: t
        integer :: x = 0
    end type
    type, extends(t) :: e
        integer :: y = 0
    end type
contains
    integer function kind_of(a, n) result(k)
        class(*), intent(in) :: a(:)
        integer, intent(in) :: n
        integer :: i
        if (size(a) /= n) error stop 1
        k = 0
        select type (a)
        type is (e)
            k = 2
            if (any(a%x /= [(10*i, i = 1, n)])) error stop 2
            if (any(a%y /= [(10*i + 1, i = 1, n)])) error stop 3
        type is (t)
            k = 1
            if (any(a%x /= [(10*i, i = 1, n)])) error stop 4
        class default
            k = -1
        end select
    end function

    subroutine check_inquiry(a, is_e)
        class(*), intent(in) :: a(:)
        logical, intent(in) :: is_e
        type(t) :: mt
        type(e) :: me
        if (same_type_as(a, mt) .neqv. .not. is_e) error stop 5
        if (same_type_as(a, me) .neqv. is_e) error stop 6
        if (.not. extends_type_of(a, mt)) error stop 7
        if (extends_type_of(a, me) .neqv. is_e) error stop 8
        if (extends_type_of(mt, a) .neqv. .not. is_e) error stop 9
    end subroutine

    integer function forward(a, n) result(k)
        class(t), intent(in) :: a(:)
        integer, intent(in) :: n
        k = kind_of(a, n)
        call check_inquiry(a, k == 2)
    end function
end module

program class_star_array_arg_01
    use class_star_array_arg_01_m
    implicit none
    class(t), allocatable :: ce(:), ct(:)
    class(t), pointer :: pe(:)
    integer :: i

    allocate(ce(3), source=[(e(10*i, 10*i + 1), i = 1, 3)])
    if (kind_of(ce, 3) /= 2) error stop 10
    call check_inquiry(ce, .true.)
    if (forward(ce, 3) /= 2) error stop 11

    allocate(ct(2), source=[(t(10*i), i = 1, 2)])
    if (kind_of(ct, 2) /= 1) error stop 12
    call check_inquiry(ct, .false.)
    if (forward(ct, 2) /= 1) error stop 13

    allocate(pe(4), source=[(e(10*i, 10*i + 1), i = 1, 4)])
    if (kind_of(pe, 4) /= 2) error stop 14
    call check_inquiry(pe, .true.)
    deallocate(pe)

    print *, "ok"
end program
