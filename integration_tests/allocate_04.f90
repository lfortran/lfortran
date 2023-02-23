program allocate_04
implicit none

    integer, allocatable :: c(:, :, :)
    character(len=:), allocatable :: string
    character(len=:), allocatable :: message
    character(len=20) :: num
    allocate(character(len=8)::string)
    allocate(character(len=20)::message)
    allocate(c(3, 3, 3))
    string = "lfortran"
    if (string /= "lfortran") error stop
    num = "lfortran working"
    message = num(1:len_trim(num)+1)
    print *, message

end program
