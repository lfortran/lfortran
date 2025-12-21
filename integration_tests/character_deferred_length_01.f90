program character_deferred_length_01
    implicit none
    
    character(2) :: x(200)
    character(:), allocatable :: y(:)
    
    ! This should work - both are deferred-length allocatable
    allocate(character(2) :: y(10))
    call ss(y)
    if (.not. allocated(y)) error stop
    if (size(y) /= 10) error stop
    if (len(y) /= 2) error stop
    
    ! Note: call ss(x) should fail with a type mismatch error
    ! because x is fixed-length and ss expects deferred-length
    ! This is tested separately to verify the error is caught
    
contains
    subroutine ss(xx)
        character(:), allocatable :: xx(:)
        ! This subroutine expects deferred-length allocatable strings
    end subroutine ss
end program

