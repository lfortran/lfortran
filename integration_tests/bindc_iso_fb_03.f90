! Consolidated ISO_Fortran_binding test: Derived Types, Characters & Array Sections
! Merged from: bindc_22, bindc_26, bindc_27
!
! bindc_22: Array sections, derived-type arrays, non-default bounds
!   - Negative stride array sections
!   - 2D/3D array sections passed as assumed-shape
!   - TYPE, BIND(C) arrays via descriptor
!   - Arrays with non-default lower bounds
!
! bindc_26: Derived type extensions through descriptors
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
!
! bindc_27: Character interop through descriptors
!   - character(c_char, len=1) 1D/2D arrays through descriptor
!   - character array sections through descriptor
!   - character inout (modify in C)

! ============================================================
! Modules from bindc_22
! ============================================================
module bindc_22_types
    use iso_c_binding, only: c_int32_t, c_double
    implicit none

    type, bind(C) :: point_t
        integer(c_int32_t) :: x
        integer(c_int32_t) :: y
    end type
end module

module bindc_22_mod
    use iso_c_binding, only: c_int, c_int32_t, c_double
    use bindc_22_types
    implicit none

    interface
        ! ---- negative stride ----
        integer(c_int32_t) function c22_sum_1d(a) bind(C, name="c22_sum_1d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        ! ---- 2D section ----
        integer(c_int32_t) function c22_sum_2d(a) bind(C, name="c22_sum_2d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:)
        end function

        ! ---- derived type array via descriptor ----
        integer(c_int32_t) function c22_sum_points(pts) &
                bind(C, name="c22_sum_points")
            import :: c_int32_t, point_t
            type(point_t), intent(in) :: pts(:)
        end function

        integer(c_int) function c22_point_elem_size(pts) &
                bind(C, name="c22_point_elem_size")
            import :: c_int, point_t
            type(point_t), intent(in) :: pts(:)
        end function

        ! ---- non-default lower bounds ----
        integer(c_int32_t) function c22_sum_nondefault(a) &
                bind(C, name="c22_sum_nondefault")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        integer(c_int) function c22_get_extent(a) &
                bind(C, name="c22_get_extent")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function
    end interface
end module

! ============================================================
! Modules from bindc_26
! ============================================================
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

! ============================================================
! Module from bindc_27
! ============================================================
module bindc_27_mod
    use iso_c_binding, only: c_int, c_char, c_int32_t
    implicit none

    interface
        ! ---- character(len=1) 1D sum ----
        integer(c_int) function c27_char_sum_1d(a) &
                bind(C, name="c27_char_sum_1d")
            import :: c_int, c_char
            character(kind=c_char, len=1), intent(in) :: a(:)
        end function

        ! ---- character(len=1) 2D sum ----
        integer(c_int) function c27_char_sum_2d(a) &
                bind(C, name="c27_char_sum_2d")
            import :: c_int, c_char
            character(kind=c_char, len=1), intent(in) :: a(:,:)
        end function

        ! ---- character inout (modify in C) ----
        subroutine c27_char_toupper(a) bind(C, name="c27_char_toupper")
            import :: c_char
            character(kind=c_char, len=1), intent(inout) :: a(:)
        end subroutine
    end interface
end module

! ============================================================
! Main program
! ============================================================
program bindc_iso_fb_03
    use bindc_22_mod
    use bindc_22_types
    use bindc_26_mod
    use bindc_26_types
    use bindc_27_mod
    use iso_c_binding, only: c_int32_t, c_char
    implicit none

    ! Tests from bindc_22
    call test_negative_stride()
    call test_2d_sections()
    call test_derived_type_array()
    call test_nondefault_bounds()

    ! Tests from bindc_26
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

    ! Tests from bindc_27
    call test_char_1d()
    call test_char_2d()
    call test_char_section()
    call test_char_inout()

    print *, "All bindc_iso_fb_03 tests passed."

contains

    ! ============================================================
    ! Test subroutines from bindc_22
    ! ============================================================

    subroutine test_negative_stride()
        integer(c_int32_t) :: arr(6)

        arr = [1, 2, 3, 4, 5, 6]

        ! Reverse: [6, 5, 4, 3, 2, 1]
        if (c22_sum_1d(arr(6:1:-1)) /= 21) error stop "FAIL: neg stride sum"

        ! Reverse stride-2: [6, 4, 2]
        if (c22_sum_1d(arr(6:1:-2)) /= 12) error stop "FAIL: neg stride-2"
    end subroutine

    subroutine test_2d_sections()
        integer(c_int32_t) :: a(4,4)
        integer :: i, j

        do j = 1, 4
            do i = 1, 4
                a(i,j) = (j-1)*4 + i
            end do
        end do

        ! Take a 2x2 sub-block: a(2:3, 2:3) = [[6,7],[10,11]]
        if (c22_sum_2d(a(2:3, 2:3)) /= 34) error stop "FAIL: 2d section"

        ! Take stride-2 in first dim: a(1::2, :) = rows 1,3 for all cols
        ! = [1,3,5,7,9,11,13,15] -> sum = 64
        if (c22_sum_2d(a(1::2, :)) /= 64) error stop "FAIL: 2d stride section"
    end subroutine

    subroutine test_derived_type_array()
        type(point_t) :: pts(3)
        integer(c_int32_t) :: expected_size

        pts(1) = point_t(1, 10)
        pts(2) = point_t(2, 20)
        pts(3) = point_t(3, 30)

        ! sum of x+y for all points = (1+10) + (2+20) + (3+30) = 66
        if (c22_sum_points(pts) /= 66) error stop "FAIL: derived type sum"

        ! elem_len should be sizeof(point_t) = 2 * sizeof(int32) = 8
        expected_size = 8
        if (c22_point_elem_size(pts) /= expected_size) &
            error stop "FAIL: derived type elem_size"
    end subroutine

    subroutine test_nondefault_bounds()
        integer(c_int32_t) :: a0(0:4)    ! 5 elements, 0-based
        integer(c_int32_t) :: an(-2:2)   ! 5 elements, negative-based
        integer :: i

        do i = 0, 4
            a0(i) = i + 1
        end do
        ! a0 = [1, 2, 3, 4, 5], sum = 15
        if (c22_sum_nondefault(a0) /= 15) error stop "FAIL: 0-based sum"
        if (c22_get_extent(a0) /= 5) error stop "FAIL: 0-based extent"

        do i = -2, 2
            an(i) = i + 3
        end do
        ! an = [1, 2, 3, 4, 5], sum = 15
        if (c22_sum_nondefault(an) /= 15) error stop "FAIL: neg-based sum"
        if (c22_get_extent(an) /= 5) error stop "FAIL: neg-based extent"
    end subroutine

    ! ============================================================
    ! Test subroutines from bindc_26
    ! ============================================================

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

    ! ============================================================
    ! Test subroutines from bindc_27
    ! ============================================================

    subroutine test_char_1d()
        character(kind=c_char, len=1) :: c1(4)
        c1 = ['A', 'B', 'C', 'D']
        ! 65 + 66 + 67 + 68 = 266
        if (c27_char_sum_1d(c1) /= 266) error stop "FAIL: char 1d sum"
    end subroutine

    subroutine test_char_2d()
        character(kind=c_char, len=1) :: c2(2, 3)
        c2(1, 1) = 'A'
        c2(2, 1) = 'B'
        c2(1, 2) = 'C'
        c2(2, 2) = 'D'
        c2(1, 3) = 'E'
        c2(2, 3) = 'F'
        ! 65+66+67+68+69+70 = 405
        if (c27_char_sum_2d(c2) /= 405) error stop "FAIL: char 2d sum"
    end subroutine

    subroutine test_char_section()
        character(kind=c_char, len=1) :: arr(6)
        arr = ['A', 'B', 'C', 'D', 'E', 'F']
        ! stride-2: A, C, E = 65 + 67 + 69 = 201
        if (c27_char_sum_1d(arr(1::2)) /= 201) &
            error stop "FAIL: char section"
    end subroutine

    subroutine test_char_inout()
        character(kind=c_char, len=1) :: arr(3)
        arr = ['a', 'b', 'c']
        call c27_char_toupper(arr)
        if (arr(1) /= 'A') error stop "FAIL: toupper 1"
        if (arr(2) /= 'B') error stop "FAIL: toupper 2"
        if (arr(3) /= 'C') error stop "FAIL: toupper 3"
    end subroutine

end program
