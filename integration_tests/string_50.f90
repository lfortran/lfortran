! Test using kind=8 with character allocation 
program string_50
    character(:), allocatable :: ret
    integer(8) :: i
    i = 10
    allocate(character(i) :: ret)
    print *, len(ret)
    if(len(ret) /= 10) error stop 
    ret  = "Hello"
    print *, len(ret)
    if(len(ret) /= 5  ) error stop 
end program 