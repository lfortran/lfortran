program function_62
    implicit none

    if (make_value(1) /= "x") error stop
    print *, "test passed"
contains

    function make_value(i)
        integer, intent(in) :: i
        character(1), parameter :: names(1) = ["x"]
        character(len_trim(names(i))) :: make_value

        make_value = "x"
    end function make_value

end program function_62
