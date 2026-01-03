module string_56_mod
    implicit none
    public :: global_names
    character(len=:), allocatable :: global_names(:)
contains 
    function get_message() result(msg)
        character(:), allocatable :: msg
        msg = "World"
    end function get_message
end module string_56_mod

program string_56
    use string_56_mod
    character(:), allocatable :: current_argument
    current_argument = "Hello"
    global_names=[character(len=5) :: current_argument]
    if (global_names(1) /= "Hello") error stop
    select case(get_message())
    case("World")
        current_argument = "World"
    end select
    if (current_argument /= "World") error stop
end program