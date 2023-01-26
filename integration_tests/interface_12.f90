subroutine find_fit(expr)
    interface
        function expr() result(y)
        implicit none
        real :: y(1)
        end function
    end interface
end subroutine
        
program main
    implicit none
    call find_fit(expression)
    contains
    function expression() result(y)
    real :: y(1)
    end function
end program
