program select_type_07
    implicit none

    class(*), allocatable :: obj

    type :: structType
        integer :: code
        character(len=20) :: message
    end type structType

    type(structType) :: err
    integer :: y = 0

    call print_obj(42, y)
    if (y /= 1) error stop
    call print_obj(3.14, y)
    if (y /= 2) error stop
    call print_obj("Hello", y)
    if (y /= 3) error stop

    err%code = 500
    err%message = "Server Error"
    call print_obj(err, y)
    if (y /= 4) error stop

contains

    subroutine print_obj(x, y)
        class(*), intent(in) :: x
        integer, intent(out) :: y
        integer :: val1
        real :: val2
        character(len=:), allocatable :: val3
        type(structType) :: val4

        select type(x)
        type is (integer)
            val1 = x
            print *, val1
            if (val1 /= 42) error stop
            y = 1
        type is (real)
            val2 = x
            print *, val2
            if (val2 /= 3.14) error stop
            y = 2
        type is (character(*))
            call check_char(x, y)
        type is (structType)
            val4 = x
            print *, val4%code
            print *, val4%message
            if (val4%code /= 500) error stop
            if (val4%message /= "Server Error") error stop
            y = 4
        end select
    end subroutine print_obj

    subroutine check_char(val, y)
        character(len=*)  :: val
        integer, intent(out) :: y
        print *, val
        if (val /= "Hello") error stop
        y = 3
    end subroutine check_char

end program select_type_07
