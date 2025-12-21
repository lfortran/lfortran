program character_deferred_length_mismatch
    implicit none
    character(2) :: x(200)
    character(:), allocatable :: y(:)
    
    ! This should work - both are deferred-length allocatable
    allocate(character(2) :: y(10))
    call ss(y)
    
    ! This should ERROR - x is fixed-length, but ss expects deferred-length
    call ss(x)
    
contains
    subroutine ss(xx)
        character(:), allocatable :: xx(:)
    end subroutine ss
end program

