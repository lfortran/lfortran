!Test global array of strings
module arrays_99_mod
    character(10) :: arr(2)
end module 
program arrays_99
use arrays_99_mod
    arr(1) = "aa"
    print *, arr(1)
    if(arr(1) /= "aa") error stop
end program