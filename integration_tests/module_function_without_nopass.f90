!> This test file makes sure that optional arguments
!> are passed correctly for StructMethodDeclaration with/without nopass
module modules_module_function_without_nopass
    implicit none
    private
    public :: calculator, SQUARE  ! Expose both the type and the SQUARE function

    type :: calculator
    contains
        procedure :: SQUARE  ! Associate SQUARE with the calculator type
        procedure :: AREA
    end type calculator

contains

    ! Define SQUARE as a module procedure.
    function SQUARE(this, x) result(square_result)
        class(calculator), intent(in) :: this
        integer, optional, intent(in) :: x
        integer :: square_result

        if (present(x)) then
            square_result = x * x
        else
            square_result = 1
        end if
    end function SQUARE

    function AREA(this, x, y) result(area_result)
        class(calculator), intent(in) :: this
        integer, optional, intent(in) :: x
        integer, optional, intent(in) :: y
        integer :: area_result

        if (present(x) .and. present(y)) then
            area_result = x * y
        else
            area_result = 1
        end if
    end function

end module modules_module_function_without_nopass

program module_function_without_nopass
    use modules_module_function_without_nopass
    implicit none
    type(calculator) :: calc
    integer :: number, result, area1, area2, area3

    number = 4

    result = calc%SQUARE(number)
    if (result /= 16) error stop

    area1 = calc%AREA()
    if (area1 /= 1) error stop

    area2 = calc%AREA(1, 2)
    if (area2 /= 2) error stop

    area3 = calc%AREA(1)
    if (area3 /= 1) error stop
end program module_function_without_nopass
