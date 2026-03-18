! Test: Derived type extensions through descriptors
!
! Covers gap items E.1-E.11:
!   - Nested bind(C) derived types through descriptors
!   - Derived type with array components
!   - Allocatable array of derived types
!   - Pointer array of derived types
!   - 2D array of derived types
!   - Assumed-rank with derived types
!   - Empty bind(C) type
!   - Derived type with character component
!   - Derived type with complex component
!   - Derived type with logical component
!   - Array section of derived type array
module bindc_26_types
    use iso_c_binding, only: c_int32_t, c_double, c_float, c_bool, c_char, &
                             c_float_complex
    implicit none

    type, bind(C) :: inner_t
        integer(c_int32_t) :: x
        integer(c_int32_t) :: y
    end type

    type, bind(C) :: nested_t
        type(inner_t) :: pos
        integer(c_int32_t) :: id
    end type

    type, bind(C) :: complex_member_t
        complex(c_float_complex) :: z
        integer(c_int32_t) :: tag
    end type

    type, bind(C) :: bool_member_t
        logical(c_bool) :: flag
        integer(c_int32_t) :: val
    end type

    type, bind(C) :: char_member_t
        character(kind=c_char, len=1) :: code
        integer(c_int32_t) :: val
    end type

    type, bind(C) :: empty_t
    end type
end module

module bindc_26_mod
    use iso_c_binding, only: c_int, c_int32_t
    use bindc_26_types
    implicit none

    interface
        ! ---- nested bind(C) type ----
        integer(c_int32_t) function c26_sum_nested(a) &
                bind(C, name="c26_sum_nested")
            import :: c_int32_t, nested_t
            type(nested_t), intent(in) :: a(:)
        end function

        ! ---- allocatable derived type array ----
        integer(c_int32_t) function c26_sum_alloc_nested(a) &
                bind(C, name="c26_sum_alloc_nested")
            import :: c_int32_t, nested_t
            type(nested_t), allocatable, intent(in) :: a(:)
        end function

        ! ---- pointer derived type array ----
        integer(c_int32_t) function c26_sum_ptr_nested(a) &
                bind(C, name="c26_sum_ptr_nested")
            import :: c_int32_t, nested_t
            type(nested_t), pointer, intent(in) :: a(:)
        end function

        ! ---- 2D derived type array ----
        integer(c_int32_t) function c26_sum_2d_inner(a) &
                bind(C, name="c26_sum_2d_inner")
            import :: c_int32_t, inner_t
            type(inner_t), intent(in) :: a(:,:)
        end function

        ! ---- assumed-rank derived type ----
        integer(c_int) function c26_rank_inner(a) &
                bind(C, name="c26_rank_inner")
            import :: c_int, inner_t
            type(inner_t), intent(in) :: a(..)
        end function

        ! ---- empty type ----
        integer(c_int) function c26_sizeof_empty(a) &
                bind(C, name="c26_sizeof_empty")
            import :: c_int, empty_t
            type(empty_t), intent(in) :: a(:)
        end function

        ! ---- derived type with complex component ----
        integer(c_int32_t) function c26_sum_complex_member(a) &
                bind(C, name="c26_sum_complex_member")
            import :: c_int32_t, complex_member_t
            type(complex_member_t), intent(in) :: a(:)
        end function

        ! ---- derived type with logical component ----
        integer(c_int) function c26_count_flagged(a) &
                bind(C, name="c26_count_flagged")
            import :: c_int, bool_member_t
            type(bool_member_t), intent(in) :: a(:)
        end function

        ! ---- derived type with character component ----
        integer(c_int) function c26_sum_char_codes(a) &
                bind(C, name="c26_sum_char_codes")
            import :: c_int, char_member_t
            type(char_member_t), intent(in) :: a(:)
        end function

        ! ---- array section of derived type ----
        integer(c_int32_t) function c26_sum_inner_section(a) &
                bind(C, name="c26_sum_inner_section")
            import :: c_int32_t, inner_t
            type(inner_t), intent(in) :: a(:)
        end function
    end interface
end module

program bindc_26
    use bindc_26_mod
    use bindc_26_types
    use iso_c_binding, only: c_int32_t
    implicit none

    call test_nested()
    call test_alloc_nested()
    call test_ptr_nested()
    call test_2d_inner()
    call test_assumed_rank_dt()
    call test_empty_type()
    call test_complex_member()
    call test_bool_member()
    call test_char_member()
    call test_dt_section()

    print *, "All bindc_26 tests passed."

contains

    subroutine test_nested()
        type(nested_t) :: arr(3)
        arr(1) = nested_t(inner_t(1, 2), 10)
        arr(2) = nested_t(inner_t(3, 4), 20)
        arr(3) = nested_t(inner_t(5, 6), 30)
        ! sum = (1+2+10) + (3+4+20) + (5+6+30) = 81
        if (c26_sum_nested(arr) /= 81) error stop "FAIL: nested sum"
    end subroutine

    subroutine test_alloc_nested()
        type(nested_t), allocatable :: arr(:)
        allocate(arr(2))
        arr(1) = nested_t(inner_t(10, 20), 100)
        arr(2) = nested_t(inner_t(30, 40), 200)
        ! sum = (10+20+100) + (30+40+200) = 400
        if (c26_sum_alloc_nested(arr) /= 400) &
            error stop "FAIL: alloc nested sum"
        deallocate(arr)
    end subroutine

    subroutine test_ptr_nested()
        type(nested_t), target :: tgt(2)
        type(nested_t), pointer :: p(:)
        tgt(1) = nested_t(inner_t(1, 1), 1)
        tgt(2) = nested_t(inner_t(2, 2), 2)
        p => tgt
        ! sum = (1+1+1) + (2+2+2) = 9
        if (c26_sum_ptr_nested(p) /= 9) error stop "FAIL: ptr nested sum"
    end subroutine

    subroutine test_2d_inner()
        type(inner_t) :: arr(2, 3)
        integer :: i, j, k
        k = 1
        do j = 1, 3
            do i = 1, 2
                arr(i, j) = inner_t(k, k * 10)
                k = k + 1
            end do
        end do
        ! x: 1..6, y: 10..60 step 10
        ! sum = (1+2+3+4+5+6) + (10+20+30+40+50+60) = 21 + 210 = 231
        if (c26_sum_2d_inner(arr) /= 231) error stop "FAIL: 2d inner sum"
    end subroutine

    subroutine test_assumed_rank_dt()
        type(inner_t) :: a1(3), a2(2, 2)
        a1(1) = inner_t(1, 1)
        a1(2) = inner_t(2, 2)
        a1(3) = inner_t(3, 3)
        if (c26_rank_inner(a1) /= 1) error stop "FAIL: ar dt rank 1"

        a2(1, 1) = inner_t(1, 1)
        a2(2, 1) = inner_t(2, 2)
        a2(1, 2) = inner_t(3, 3)
        a2(2, 2) = inner_t(4, 4)
        if (c26_rank_inner(a2) /= 2) error stop "FAIL: ar dt rank 2"
    end subroutine

    subroutine test_empty_type()
        type(empty_t) :: arr(3)
        integer :: sz
        sz = c26_sizeof_empty(arr)
        ! empty type has implementation-defined size, just check it runs
        if (sz < 0) error stop "FAIL: empty type"
    end subroutine

    subroutine test_complex_member()
        type(complex_member_t) :: arr(2)
        arr(1) = complex_member_t((1.0, 2.0), 10)
        arr(2) = complex_member_t((3.0, 4.0), 20)
        ! sum of tags = 10 + 20 = 30
        if (c26_sum_complex_member(arr) /= 30) &
            error stop "FAIL: complex member"
    end subroutine

    subroutine test_bool_member()
        type(bool_member_t) :: arr(4)
        arr(1) = bool_member_t(.true., 10)
        arr(2) = bool_member_t(.false., 20)
        arr(3) = bool_member_t(.true., 30)
        arr(4) = bool_member_t(.false., 40)
        ! count flagged = 2
        if (c26_count_flagged(arr) /= 2) error stop "FAIL: bool member"
    end subroutine

    subroutine test_char_member()
        type(char_member_t) :: arr(3)
        arr(1) = char_member_t('A', 1)
        arr(2) = char_member_t('B', 2)
        arr(3) = char_member_t('C', 3)
        ! sum of char codes = 65 + 66 + 67 = 198
        if (c26_sum_char_codes(arr) /= 198) error stop "FAIL: char member"
    end subroutine

    subroutine test_dt_section()
        type(inner_t) :: arr(6)
        integer :: i
        do i = 1, 6
            arr(i) = inner_t(i, i * 10)
        end do
        ! stride-2 section: elements 1, 3, 5
        ! sum = (1+10) + (3+30) + (5+50) = 99
        if (c26_sum_inner_section(arr(1::2)) /= 99) &
            error stop "FAIL: dt section"
    end subroutine

end program
