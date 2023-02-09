module add_m
    implicit none
    private

contains

    integer function func_arg_int(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

    subroutine test()
        real :: x, y
        x = 5.1
        y = 7.2
        print*, "The result is ", func_arg_int(x, y)
    end subroutine
end module

program add
use add_m
implicit none

call test()

end program add
