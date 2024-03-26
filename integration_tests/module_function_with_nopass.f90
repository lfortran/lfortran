module calculator_mod
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

end module calculator_mod

program CalculateSquare
    use calculator_mod
    implicit none
    type(calculator) :: calc
    integer :: number, result

    number = 4

    result = calc%SQUARE(number)
    print *, 'The square of ', number, ' is ', result
end program CalculateSquare
