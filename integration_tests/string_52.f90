! This check if the string is copied correctly and not just the reference
program string_52
    type tt
      character(10) :: str
    end type tt

    character(10) :: fixed_s
    type(tt) :: inst
    
    fixed_s = "helloworld"
    inst%str = fixed_s
    print *, inst%str
    print *, fixed_s

    if(inst%str /= "helloworld") error stop
    if(fixed_s /= "helloworld") error stop
    
    ! Change one, and make sure the other one doesn't reference it 
    fixed_s = "0123456789" 
    
    print *, inst%str
    print *, fixed_s
    if(inst%str /= "helloworld") error stop
    if(fixed_s /= "0123456789") error stop
end program 