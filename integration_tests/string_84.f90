program string_84
    implicit none
    character(len=:), pointer :: str
    character(5), target :: temp_str
    temp_str = "Hello"
    str => temp_str
    str(2:2) = "?"
  
    print *, temp_str
    if(temp_str /= "H?llo") error stop
  
    if(.not. associated(str)) error stop 
    nullify(str)
    if(associated(str)) error stop 
end program 