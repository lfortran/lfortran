program mre_parameter1
    implicit none
    character(3), parameter :: ar1 = repeat(["abc", "#^1", "123"], [1, 2, 3])
    print *, ar1
end program
