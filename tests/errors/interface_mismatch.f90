module mismatch_function
    implicit none

    interface

        module function f() result(i)
            integer :: i
        end function
    end interface

contains


    module function f() result(r)
        real :: r
        r = 5.5
    end function

end module

program test
    use mismatch_function
end program
