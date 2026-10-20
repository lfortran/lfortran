! Fortran 2023 conditional expressions with character arms used as operands
! in the test of an IF or DO WHILE, and in PRINT and WRITE: the result string
! must be freed after the statement (checked by the --detect-leaks mode).
!
! This test is not labelled `gfortran`, for the same reason as
! conditional_expr_01 to 08 (see integration_tests/CMakeLists.txt).
program conditional_expr_09
    implicit none
    logical :: f
    integer :: i
    character(8) :: buf

    f = .false.
    if (( f ? "yes" : "no" ) /= "no") error stop 1
    if (( .not. f ? "yes" : "no" ) /= "yes") error stop 2

    i = 0
    do while (( i < 3 ? "go" : "stop" ) == "go")
        i = i + 1
    end do
    if (i /= 3) error stop 3

    print *, ( f ? "yes" : "no" )
    write (buf, "(a)") ( .not. f ? "yes" : "no" )
    if (buf /= "yes") error stop 4
end program
