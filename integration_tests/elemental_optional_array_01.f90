module elemental_optional_array_01_m
    implicit none
    type :: e_t
        integer :: i = 0
        integer :: n = 0
    end type
contains
    impure elemental function ec(i, l) result(e)
        integer, intent(in) :: i
        character(len=*), intent(in), optional :: l
        type(e_t) :: e
        e%i = i
        e%n = -1
        if (present(l)) e%n = len(l)
    end function

    impure elemental function ei(i, k) result(r)
        integer, intent(in) :: i
        integer, intent(in), optional :: k
        integer :: r
        r = i
        if (present(k)) r = r + k
    end function

    subroutine s_char(ed, l, r)
        integer, intent(in) :: ed(:)
        character(len=*), dimension(:), intent(in), optional :: l
        type(e_t), allocatable, intent(out) :: r(:)
        allocate(r(size(ed)))
        r = ec(i=ed, l=l)
    end subroutine

    subroutine s_int(ed, k, r)
        integer, intent(in) :: ed(:)
        integer, intent(in), optional :: k(:)
        integer, intent(out) :: r(:)
        r = ei(ed, k)
    end subroutine
end module

program elemental_optional_array_01
    use elemental_optional_array_01_m
    implicit none
    type(e_t), allocatable :: r(:)
    integer :: ri(3)

    call s_char([1, 2, 3], r=r)
    if (size(r) /= 3) error stop 1
    if (any(r%i /= [1, 2, 3])) error stop 2
    if (any(r%n /= -1)) error stop 3
    deallocate(r)

    call s_char([1, 2, 3], ["ab ", "abc", "a  "], r)
    if (any(r%i /= [1, 2, 3])) error stop 4
    if (any(r%n /= 3)) error stop 5

    call s_int([1, 2, 3], r=ri)
    if (any(ri /= [1, 2, 3])) error stop 6
    call s_int([1, 2, 3], [10, 20, 30], ri)
    if (any(ri /= [11, 22, 33])) error stop 7
end program
