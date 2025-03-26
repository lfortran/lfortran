program intrinsics
    implicit none

    integer, dimension(5) :: value  
    integer :: val, iloc

    value = [0, 2, 1, 3, 1]  
    val = 1                  

    iloc = findloc(value, val, dim=1)  

    print * , iloc
    if (iloc /= 3) error stop 
end program