program string_87
    character(10) :: str
    str = "HelloWorld"
    call ff(str)
    if(str /= "HelloWTrld") error stop 3 
    contains
    
    subroutine ff(s)
      character(5) :: s(2)
      if(s(1) /= "Hello") error stop 1
      if(s(2) /= "World") error stop 2
      s(2)(2:2) = "T"
    end subroutine
end program 