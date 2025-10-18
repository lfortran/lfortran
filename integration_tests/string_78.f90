! Test is specially made to handle the nesting of the concatenation operator
! as it could lead to performance issues if not handled properly.
program string_78
    character(:), allocatable :: ss
    character(10) :: str
    str = "HELLOWORLD"
    allocate(character(300) :: ss)
    ss = str // str // str // str // str // str // str // str // str //&
                & str // str // str // str // str // str // str // str //&
                & str // str // str // str // str // str // str // str // str // str // str // str // str
    print *, ss 
    !! HELLOWROLD * 30
    if(ss /= &
             &"HELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLD&
             &HELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLD&
             &HELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLDHELLOWORLD") error stop
end program