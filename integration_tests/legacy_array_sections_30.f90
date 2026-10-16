module legacy_array_sections_30_mod
    ! With --legacy-array-sections, an element passed to an assumed-shape
    ! dummy of a use-associated procedure is rewritten to the section that
    ! starts at the element, w(i) -> w(i:), as for a local procedure (#15332).
    implicit none
contains
    subroutine int_in(x, n, first)
        integer, intent(in) :: x(:)
        integer, intent(in) :: n, first
        integer :: i
        if (size(x) /= n) error stop 1
        do i = 1, n
            if (x(i) /= first + i - 1) error stop 2
        end do
    end subroutine

    subroutine int_negate(x)
        integer, intent(inout) :: x(:)
        x = -x
    end subroutine

    subroutine char_fixed(x, n, expected)
        character(len=3), intent(in) :: x(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: expected
        integer :: i
        if (size(x) /= n) error stop 3
        do i = 1, n
            if (x(i) /= expected(3*i-2:3*i)) error stop 4
        end do
    end subroutine

    subroutine char_assumed_len(x, n, expected)
        character(len=*), intent(in) :: x(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: expected
        integer :: i
        if (size(x) /= n) error stop 5
        if (len(x) /= 3) error stop 6
        do i = 1, n
            if (x(i) /= expected(3*i-2:3*i)) error stop 7
        end do
    end subroutine

    subroutine char_upper(x)
        character(len=3), intent(inout) :: x(:)
        x(1) = "XXX"
    end subroutine
end module

program legacy_array_sections_30
    ! GFortran rejects these calls (rank mismatch: scalar element passed to
    ! an assumed-shape dummy), so this test is registered for LFortran only.
    use legacy_array_sections_30_mod, only: int_in, int_negate, char_fixed, &
        char_assumed_len, char_upper
    implicit none
    integer :: c(5)
    integer, allocatable :: ca(:)
    character(len=3) :: d(4)
    character(len=3), allocatable :: da(:)
    integer :: i

    c = [(i, i = 1, 5)]
    call int_in(c(2), 4, 2)
    call int_negate(c(3))
    if (any(c /= [1, 2, -3, -4, -5])) error stop 10

    allocate(ca(5))
    ca = [(i, i = 1, 5)]
    call int_in(ca(2), 4, 2)
    call int_negate(ca(4))
    if (any(ca /= [1, 2, 3, -4, -5])) error stop 11

    d = ["aaa", "bbb", "ccc", "ddd"]
    call char_fixed(d(2), 3, "bbbcccddd")
    call char_assumed_len(d(2), 3, "bbbcccddd")
    call char_upper(d(3))
    if (any(d /= ["aaa", "bbb", "XXX", "ddd"])) error stop 12

    allocate(da(4))
    da = ["aaa", "bbb", "ccc", "ddd"]
    call char_fixed(da(2), 3, "bbbcccddd")
    call char_assumed_len(da(3), 2, "cccddd")
    call char_upper(da(2))
    if (any(da /= ["aaa", "XXX", "ccc", "ddd"])) error stop 13

    print *, "ok"
end program
