program select_type_07
    implicit none

    class(*), allocatable :: obj

    type :: structType
        integer :: code
        character(len=20) :: message
    end type structType

    type(structType) :: err

    call print_obj(42)
    call print_obj(3.14)

    err%code = 500
    err%message = "Server Error"
    call print_obj(err)

contains

    subroutine print_obj(x)
        class(*), intent(in) :: x
        integer :: val1
        real :: val2
        ! character(len=:), allocatable :: val3  ! TODO
        type(structType) :: val4

        select type(x)
        type is (integer)
            val1 = x
            print *, val1
            if (val1 /= 42) error stop
        type is (real)
            val2 = x
            print *, val2
            if (val2 /= 3.14) error stop
        type is (structType)
            val4 = x
            print *, val4%code
            print *, val4%message
            if (val4%code /= 500) error stop
            if (val4%message /= "Server Error") error stop
        end select
    end subroutine print_obj

end program select_type_07
