program minpack_03

    implicit none
    integer,dimension(2),parameter :: info_original = [1,1]
    integer :: ic
    ic = 1

    call compare_solutions(ic)

    contains

    subroutine compare_solutions(ic)

    implicit none

    integer :: ic
    
    if ( info_original(1) /= 1 ) error stop
    if ( info_original(2) /= 1 ) error stop
    if ( size(info_original) /= 2 ) error stop

    print *, info_original(ic)
    end subroutine compare_solutions

end program minpack_03
