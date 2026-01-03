program arrays_39
implicit none


real, allocatable :: array(:)
integer :: ndim

ndim = 10

allocate(array(-(ndim/2):ndim))

print *, lbound(array > 0, 1)
call f(array > 0)

contains

    subroutine f(array1)
        logical, intent(in) :: array1(:)
        print *, lbound(array1, 1), ubound(array1, 1)
        if( lbound(array1, 1) /= 1 ) error stop
        if( ubound(array1, 1) /= 16 ) error stop
    end subroutine

end program arrays_39
