!> test that user-defined elemental subroutines
!> get broadcasted correctly
module elemental_09_module
implicit none
contains

    elemental subroutine square_element(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x * x
    end subroutine square_element

    elemental subroutine multiply_elements(x, y, z)
        real, intent(in) :: x
        real, intent(in) :: y
        real, intent(out) :: z
        z = x * y
    end subroutine multiply_elements

end module elemental_09_module


program elemental_09
    use elemental_09_module
    implicit none
    real, parameter :: epsilon = 1e-7
    real, dimension(5) :: arr_in1 = [1.0, 2.0, 3.0, 4.0, 5.0]
    real, dimension(5) :: arr_in2 = [2.0, 3.0, 4.0, 5.0, 6.0]
    real, dimension(5) :: arr_out

    call square_element(arr_in1, arr_out)
    print *, arr_out
    if (abs(arr_out(1) - 1) > epsilon ) error stop
    if (abs(arr_out(2) - 4) > epsilon ) error stop
    if (abs(arr_out(3) - 9) > epsilon ) error stop
    if (abs(arr_out(4) - 16) > epsilon ) error stop
    if (abs(arr_out(5) - 25) > epsilon ) error stop

    call multiply_elements(arr_in1, arr_in2, arr_out)
    print *, arr_out
    if (abs(arr_out(1) - 2) > epsilon ) error stop
    if (abs(arr_out(2) - 6) > epsilon ) error stop
    if (abs(arr_out(3) - 12) > epsilon ) error stop
    if (abs(arr_out(4) - 20) > epsilon ) error stop
    if (abs(arr_out(5) - 30) > epsilon ) error stop
    print *, arr_out

    call multiply_elements(arr_in1, 2.0, arr_out)
    print *, arr_out
    if (abs(arr_out(1) - 2) > epsilon ) error stop
    if (abs(arr_out(2) - 4) > epsilon ) error stop
    if (abs(arr_out(3) - 6) > epsilon ) error stop
    if (abs(arr_out(4) - 8) > epsilon ) error stop
    if (abs(arr_out(5) - 10) > epsilon ) error stop

    call multiply_elements(3.0, arr_in1, arr_out)
    print *, arr_out
    if (abs(arr_out(1) - 3) > epsilon ) error stop
    if (abs(arr_out(2) - 6) > epsilon ) error stop
    if (abs(arr_out(3) - 9) > epsilon ) error stop
    if (abs(arr_out(4) - 12) > epsilon ) error stop
    if (abs(arr_out(5) - 15) > epsilon ) error stop

    call sum_elements(arr_in1, arr_in2, arr_out)
    print *, arr_out
    if (abs(arr_out(1) - 3) > epsilon ) error stop
    if (abs(arr_out(2) - 5) > epsilon ) error stop
    if (abs(arr_out(3) - 7) > epsilon ) error stop
    if (abs(arr_out(4) - 9) > epsilon ) error stop
    if (abs(arr_out(5) - 11) > epsilon ) error stop

    contains

    elemental subroutine sum_elements(x, y, z)
        real, intent(in) :: x
        real, intent(in) :: y
        real, intent(out) :: z
        z = x + y
    end subroutine sum_elements
end program elemental_09
