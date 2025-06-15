module string_56_mod
    implicit none
    public :: global_names
    character(len=:), allocatable :: global_names(:)
end module string_56_mod

program string_56
    use string_56_mod
    character(:), allocatable :: current_argument
    current_argument = "Hello"
    global_names=[character(len=5) :: current_argument]
    if (global_names(1) /= "Hello") error stop
end program