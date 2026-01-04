! Test COMMON block sharing between host and contained procedures
program common_25
    implicit none
    integer :: val1, val2
    common /testblock/ val1, val2

    val1 = 42
    val2 = 100

    call modify_common()

    if (val1 /= 99) error stop
    if (val2 /= 200) error stop

contains
    subroutine modify_common()
        integer :: v1, v2
        common /testblock/ v1, v2

        if (v1 /= 42) error stop
        if (v2 /= 100) error stop
        v1 = 99
        v2 = 200
    end subroutine
end program
