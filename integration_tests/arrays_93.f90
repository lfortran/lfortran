program arrays_93 
    implicit none
    integer :: result
    logical :: array(5)
    array = [ .true., .false., .true., .false., .true.]
    call falseloc(array, result)
    print *, "Number of false elements:", result
    if (result /= 2) error stop
    
contains
    subroutine falseloc(array, return_value)
        logical, intent(in) :: array(:)
        integer, intent(out) :: return_value
        integer :: loc(count(.not.(array)))
        return_value = size(loc)

    end subroutine falseloc
end program arrays_93
