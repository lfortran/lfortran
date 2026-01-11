program allocated_04
    implicit none
    integer, allocatable :: x

    allocate(x)
    allocate(x)  ! Error: double allocation of scalar
    print *, "Should not reach here"
end program allocated_04
