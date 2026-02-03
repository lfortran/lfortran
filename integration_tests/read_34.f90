program cast_test
    implicit none
    integer, parameter :: i1 = 1
    logical(kind=i1) :: arr(2)
    logical(kind=i1) :: scalar_val
    
    open(10, file='read_34.bin', access='direct', recl=2, status='replace')

    ! Test 1: Scalar Read 
    
    write(10, rec=1) .true._i1
    scalar_val = .false._i1
    read(10, rec=1) scalar_val
    if (.not. scalar_val) then
        print *, "FAILED: Scalar Read"
        error stop
    end if

    ! Test 2: Array Element Read 
    write(10, rec=2) .false._i1
    arr(1) = .true._i1
    read(10, rec=2) arr(2)
    if (arr(2)) then
        print *, "FAILED: Array Element Read"
        error stop
    end if

    close(10, status='delete')
end program