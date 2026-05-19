program test_char_realloc
    implicit none
    character(len=3), allocatable :: res(:)
    
    ! 1. Allocate to an initial size of 1
    allocate(res(1))
    res(1) = "abc"
    
    ! 2. Reallocate on assignment to a size of 2.
    ! Without the flag, LFortran will fail to resize this array.
    res = ["dog", "cat"]
    
    ! 3. Strictly check the size. If the flag is missing, this will trigger the error.
    if (size(res) /= 2) error stop "LHS reallocation failed: incorrect size"
    if (res(2) /= "cat") error stop "LHS reallocation failed: incorrect data"
    
    print *, "test passed"
end program test_char_realloc
