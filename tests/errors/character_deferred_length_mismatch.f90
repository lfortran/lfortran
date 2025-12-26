program character_deferred_length_mismatch
    implicit none
    character(2) :: x(200)
    character(:), allocatable :: y(:)
    
    allocate(character(2) :: y(10))
    call ss(y)
    
    call ss(x)
    
contains
    subroutine ss(xx)
        character(:), allocatable :: xx(:)
    end subroutine ss
end program

