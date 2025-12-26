program character_deferred_length_01
    implicit none
    
    character(2) :: x(200)
    character(:), allocatable :: y(:)
    
    allocate(character(2) :: y(10))
    call ss(y)
    if (.not. allocated(y)) error stop
    if (size(y) /= 10) error stop
    if (len(y) /= 2) error stop
    
    
contains
    subroutine ss(xx)
        character(:), allocatable :: xx(:)
    end subroutine ss
end program

