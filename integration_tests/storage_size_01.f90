program test_storage_size
    implicit none
    character(len=:), allocatable :: text
    character(len=5) :: fixed_text
    
    ! 1. Test runtime evaluation for deferred-length (allocatable) characters
    text = "x"
    if (storage_size(text) /= 8) stop 1
    
    text = "xyz"
    if (storage_size(text) /= 24) stop 2
    
    ! 2. Test compile-time evaluation for fixed-length characters
    if (storage_size(fixed_text) /= 40) stop 3
    
    ! 3. Test compile-time evaluation for fundamental types
    if (storage_size(1) /= 32) stop 4
    if (storage_size(1.0) /= 32) stop 5
    
    print *, "OK"
end program test_storage_size
