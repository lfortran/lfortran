module modules_module_function_with_nopass
    implicit none
    private
    public :: calculator, SQUARE  ! Expose both the type and the SQUARE function

    type :: calculator
    contains
        procedure, nopass :: SQUARE  ! Associate SQUARE with the calculator type
    end type calculator

contains

    ! Define SQUARE as a module procedure.
    function SQUARE(x) result(square_result)
        integer, optional, intent(in) :: x
        integer :: square_result

        if (present(x)) then
            square_result = x * x
        else
            square_result = 1
        end if
    end function SQUARE

end module modules_module_function_with_nopass

program module_function_with_nopass
    use modules_module_function_with_nopass
    implicit none
    type(calculator) :: calc
    integer :: number, result

    number = 4

    result = calc%SQUARE(number)
    if (abs(result) /= 16) error stop
end program module_function_with_nopass
