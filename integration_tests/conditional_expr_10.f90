! Fortran 2023 conditional expressions with character arms nested in an arm
! of a non-character conditional expression: the inner result string must be
! freed only when that arm is evaluated, also when the arm is not taken.
!
! This test is not labelled `gfortran`, for the same reason as
! conditional_expr_01 to 08 (see integration_tests/CMakeLists.txt). The
! taken arm is not used in PRINT or WRITE because GFortran 16.2 crashes there.
program conditional_expr_10
    implicit none
    logical :: f, t
    integer :: n, k
    character(:), allocatable :: a, b
    character(8) :: buf

    f = .false.
    t = .true.
    a = "abc  "
    b = "de"

    ! Untaken arm
    n = (f ? len_trim((t ? a : b)) : 7)
    if (n /= 7) error stop 1
    n = (t ? 8 : len_trim((t ? a : b)))
    if (n /= 8) error stop 2
    if ((f ? len_trim((t ? a : b)) : 7) /= 7) error stop 3
    print *, (f ? len_trim((t ? a : b)) : 7)
    write (buf, "(i0)") (f ? len_trim((t ? a : b)) : 7)
    if (buf /= "7") error stop 4
    k = 0
    do while ((f ? len_trim((t ? a : b)) : k) < 2)
        k = k + 1
    end do
    if (k /= 2) error stop 5

    ! Taken arm
    n = (t ? len_trim((t ? a : b)) : 7)
    if (n /= 3) error stop 6
    n = (f ? 8 : len_trim((f ? a : b)))
    if (n /= 2) error stop 7
    if ((t ? len_trim((f ? a : b)) : 7) /= 2) error stop 8
    k = 0
    do while ((t ? len_trim((t ? a : b)) + k : 0) < 5)
        k = k + 1
    end do
    if (k /= 2) error stop 9
end program
