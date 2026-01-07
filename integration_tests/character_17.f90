! Test CHARACTER(*) assumed-size arrays via EXTERNAL with various dimensions
! Comprehensive test for DescriptorArray handling in implicit interfaces:
! 1. CHARACTER(1) array with size 1 (basic case)
! 2. CHARACTER(10) array with size 3 (string length > 1, multiple elements)
! 3. 2D CHARACTER array (multi-dimensional)
program character_17
    implicit none
    character(1) :: single_char(1)
    character(10) :: multi_char(3)
    character(5) :: matrix(2, 3)
    external sub_char_1d_single
    external sub_char_1d_multi
    external sub_char_2d

    ! Test 1: Single element CHARACTER(1) array
    single_char(1) = 'A'
    call sub_char_1d_single(single_char)

    ! Test 2: CHARACTER(10) array with 3 elements
    multi_char(1) = 'First     '
    multi_char(2) = 'Second    '
    multi_char(3) = 'Third     '
    call sub_char_1d_multi(multi_char)

    ! Test 3: 2D CHARACTER(5) array
    matrix(1, 1) = 'R1C1 '
    matrix(2, 1) = 'R2C1 '
    matrix(1, 2) = 'R1C2 '
    matrix(2, 2) = 'R2C2 '
    matrix(1, 3) = 'R1C3 '
    matrix(2, 3) = 'R2C3 '
    call sub_char_2d(matrix)

    print *, "PASS"
end program

subroutine sub_char_1d_single(arr)
    implicit none
    character(*) :: arr(*)
    logical :: lsame
    external lsame

    if (.not. lsame(arr(1), 'A')) then
        error stop "Test 1 failed: arr(1) is not A"
    end if
end subroutine

subroutine sub_char_1d_multi(arr)
    implicit none
    character(*) :: arr(*)

    if (arr(1)(1:5) /= 'First') then
        error stop "Test 2 failed: arr(1) is not First"
    end if
    if (arr(2)(1:6) /= 'Second') then
        error stop "Test 2 failed: arr(2) is not Second"
    end if
    if (arr(3)(1:5) /= 'Third') then
        error stop "Test 2 failed: arr(3) is not Third"
    end if
end subroutine

subroutine sub_char_2d(arr)
    implicit none
    character(*) :: arr(2, *)

    ! Check column-major order (Fortran)
    if (arr(1, 1) /= 'R1C1 ') then
        error stop "Test 3 failed: arr(1,1) is not R1C1"
    end if
    if (arr(2, 1) /= 'R2C1 ') then
        error stop "Test 3 failed: arr(2,1) is not R2C1"
    end if
    if (arr(1, 2) /= 'R1C2 ') then
        error stop "Test 3 failed: arr(1,2) is not R1C2"
    end if
    if (arr(2, 2) /= 'R2C2 ') then
        error stop "Test 3 failed: arr(2,2) is not R2C2"
    end if
    if (arr(1, 3) /= 'R1C3 ') then
        error stop "Test 3 failed: arr(1,3) is not R1C3"
    end if
    if (arr(2, 3) /= 'R2C3 ') then
        error stop "Test 3 failed: arr(2,3) is not R2C3"
    end if
end subroutine

logical function lsame(ca, cb)
    implicit none
    character(1), intent(in) :: ca, cb
    lsame = ca == cb
end function
