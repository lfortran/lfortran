program intrinsics_401
    implicit none
    integer :: arr(3)
    integer :: result
    
    arr = [5, 10, 15]
    result = minloc(array=arr, dim=1, mask=[.false., .false., .false.])
    print*, result
    if (result /= 0) error stop "minloc with all-false mask should return 0"
    
    arr = [5, 10, 15]
    if (any(minloc(array=arr, mask=[.false., .false., .false.]) /= 0)) then
        error stop "minloc with all-false mask (array result) should return 0"
    end if

    print *, minloc(arr, .true.)
    if(any(minloc(arr, .true.) /= 1)) error stop
    print *, minloc(arr, .false.)
    if(any(minloc(arr, .false.) /= 0)) error stop
end program intrinsics_401
