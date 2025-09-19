! Test allocatable integer as substring bounds
program string_73
    character(6):: str
    integer, allocatable :: ii
    integer, allocatable :: ii2
    str = "Hello!"
  
    allocate(ii)
    allocate(ii2)
  
    ii = 6
    if(str(3:ii) /= "llo!") error stop
    
    ii2 = 3
    if(str(ii2:6) /= "llo!") error stop
  
    ii = 3
    ii2 = 6
    if(str(ii:ii2) /= "llo!") error stop
  end program string_73