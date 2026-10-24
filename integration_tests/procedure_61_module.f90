module procedure_61_module
    implicit none
    abstract interface
        function array_function(x) result(r)
            real, intent(in) :: x
            real :: r(1)
        end function
    end interface
contains
    subroutine reset(p)
        procedure(array_function), pointer :: p
        p => null()
    end subroutine

    function values(x) result(r)
        real, intent(in) :: x
        real :: r(1)
        r = x
    end function
end module
