! Test: Procedure pointer declared with type-spec (implicit interface)
program proc_ptr_16
    implicit none

    procedure(integer), pointer :: fptr => null()

    fptr => get_value
    if (fptr(5) /= 50) error stop "FAIL: expected 50"

    print *, "PASS: proc_ptr_16"

contains
    function get_value(x)
        integer, intent(in) :: x
        integer :: get_value
        get_value = x * 10
    end function
end program
