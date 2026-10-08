! A component of an array section whose extent depends on runtime values,
! such as `items(lo:hi)%nfld`, is not a fixed-size array.
module derived_types_223_mod
    implicit none
    type :: item_t
        integer :: nfld
        real :: w
    end type
contains
    integer function class_section_sum(lo, hi, items) result(r)
        integer, intent(in) :: lo, hi
        class(item_t), intent(in) :: items(:)
        r = sum(items(lo:hi)%nfld)
    end function

    integer function type_section_sum(lo, hi, items) result(r)
        integer, intent(in) :: lo, hi
        type(item_t), intent(in) :: items(:)
        r = sum(items(lo:hi)%nfld)
    end function

    real function strided_section_sum(lo, hi, step, items) result(r)
        integer, intent(in) :: lo, hi, step
        class(item_t), intent(in) :: items(:)
        r = sum(items(lo:hi:step)%w)
    end function

    subroutine reversed_section_plus_one(x, r)
        type(item_t), intent(in) :: x(:)
        class(item_t), intent(inout) :: r(:)
        r%nfld = x(size(x):1:-1)%nfld + 1
    end subroutine
end module

program derived_types_223
    use derived_types_223_mod
    implicit none
    type(item_t) :: items(4), rev(4)
    items(:)%nfld = [10, 20, 30, 40]
    items(:)%w = [1.0, 2.0, 3.0, 4.0]
    rev(:)%nfld = 0
    rev(:)%w = 0.0

    if (class_section_sum(1, 2, items) /= 30) error stop 1
    if (class_section_sum(2, 4, items) /= 90) error stop 2
    if (type_section_sum(1, 2, items) /= 30) error stop 3
    if (type_section_sum(3, 3, items) /= 30) error stop 4
    if (abs(strided_section_sum(1, 4, 2, items) - 4.0) > 1e-6) error stop 5
    if (abs(strided_section_sum(4, 1, -3, items) - 5.0) > 1e-6) error stop 6

    call reversed_section_plus_one(items, rev)
    if (any(rev%nfld /= [41, 31, 21, 11])) error stop 7
    print *, "ok"
end program
