module module_1
    implicit none
    interface operator(.mul.)
        module procedure multiply
    end interface
contains
    integer function multiply(a, b)
        integer, intent(in) :: a
        integer, intent(in) :: b
        multiply = a *  b
    end function multiply
end module module_1

module module_2
    use module_1, only: operator(.mul.)
    implicit none
contains
    subroutine compute()
        integer :: res
        res = 123 .mul. 456
        if (res /= 56088) error stop
    end subroutine compute
end module module_2

program interface_13
    use module_2
    implicit none
    call compute()
end program interface_13
