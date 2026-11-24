program test_storage_size
    implicit none
    character(len=:), allocatable :: text
    character(len=5) :: fixed_text
    type :: pair_t
        integer(8) :: key
        real :: vals(3)
    end type pair_t
    type(pair_t) :: pair, pairs(2)
    integer :: m(2, 3)
    complex :: z
    
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

    ! 4. Data objects that are not whole variables: an array element, an
    ! array section, structure components and a function result
    if (storage_size(m(1, 2)) /= 32) stop 6
    if (storage_size(m(:, 2:3)) /= 32) stop 7
    if (storage_size(pair%key) /= 64) stop 8
    if (storage_size(pair%vals(2:3)) /= 32) stop 9
    if (storage_size(pairs(2)%key, 8) /= 64_8) stop 10
    if (storage_size(fixed_text(2:4)) /= 24) stop 11
    if (storage_size(real(z)) /= 32) stop 12
    if (storage_size(2 + 3) /= 32) stop 13
    
    print *, "OK"
end program test_storage_size
