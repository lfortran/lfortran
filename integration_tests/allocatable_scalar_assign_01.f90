program allocatable_scalar_assign_01
    implicit none
    integer, allocatable :: a, b
    a = 10
    b = a
    if (b /= 10) error stop 1
    
    ! Reassign to verify if it correctly handles reallocation (or skipped since it's same shape/type)
    a = 20
    b = a
    if (b /= 20) error stop 2
    
    print *, "OK"
end program allocatable_scalar_assign_01
