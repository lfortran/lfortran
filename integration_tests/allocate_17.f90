program allocate_17
implicit none

    integer, allocatable :: array(:)
    array = arr()
    print *, array
    if( any(array /= [1]) ) error stop

contains

    function arr()
        integer arr(1)
        arr(1) = 1
    end function

end program test
