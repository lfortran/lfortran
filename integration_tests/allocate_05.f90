program allocate_05
    implicit none

    character(len=:), allocatable :: string
    character(len=3) :: num
    allocate(character(len=3)::string)
    num = "abc"
    string(:) = num
    if (string /= "abc") error stop

end program
