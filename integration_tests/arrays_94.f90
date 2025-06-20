program arrays_94
    implicit none
    integer :: result
    logical :: array(5)
    array = [ .true., .false., .true., .false., .true.]
    call truecount(array, result)
    print *, "Number of true elements:", result
    if (result /= 3) error stop
    
contains
    subroutine truecount(array, return_value)
        logical, intent(in) :: array(:)
        integer, intent(out) :: return_value
        integer :: loc(count((array .and. .true.)))
        return_value = size(loc)

    end subroutine truecount
end program arrays_94
