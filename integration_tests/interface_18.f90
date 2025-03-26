module module_interface_18
    implicit none

    abstract interface
        subroutine OBJ(x, f)
            implicit none
            real(4), intent(in) :: x(:)
            real(4), intent(out) :: f
        end subroutine OBJ
    end interface

    interface evaluate
        module procedure evaluatef
    end interface evaluate

    contains

    function moderatex(x) result(y)
        implicit none
        real, intent(in) :: x(:)
        real :: y(size(x))
        y = 2
    end function moderatex

    subroutine evaluatef(calfun, x, f)
        implicit none

        procedure(OBJ) :: calfun  
        real, intent(in) :: x(:)
        real, intent(out) :: f
        call calfun(moderatex(x), f)
    end subroutine evaluatef

    subroutine initxf(calfun, xpt)
        implicit none

        procedure(OBJ) :: calfun
        real, intent(out) :: xpt(:, :)

        real :: f
        real :: x(size(xpt, 1))

        x = 1
        f = 2
        call evaluate(calfun, x, f)
    end subroutine initxf

end module module_interface_18

program interface_18
    use module_interface_18
    implicit none

    real :: xpt(1, 5)
    
    call initxf(calfun, xpt)

contains

    subroutine calfun(x, f)
        implicit none
        real, intent(in) :: x(:)
        real, intent(out) :: f

        print * , "x = " , x
        print * , "f = " , f
        
        if (x(1) /= 2.0) error stop
        if (f /= 2.0) error stop
    end subroutine

end program
