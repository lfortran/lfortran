program optional_example
    implicit none
    real(8) :: result
    call power_function(3.0_8, result) 
    if (result /= 9.0_8) error stop 
    call power_function(3.0_8, result, 3) 
    if (result /= 27.0_8) error stop 
    call interface_trouble(5)
contains
    subroutine power_function(base, res, exponent)
        implicit none
        real(8), intent(in) :: base
        integer, intent(in), optional :: exponent
        real(8), intent(out) :: res
        integer :: exp_value
        if (present(exponent)) then
            exp_value = exponent
        else
            exp_value = 2
        end if
        res = base ** exp_value
    end subroutine power_function

    recursive subroutine interface_trouble(n,t)
        implicit none
        integer, intent(in) :: n
        real(8), intent(out), optional :: t
        if(n <= 0) return
        call interface_trouble(n-1, t)
    end subroutine interface_trouble

end program optional_example
