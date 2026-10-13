! A do while condition that calls an internal function which uses an
! allocatable array of the host (#14758).
program nested_vars_22
    implicit none
    integer, allocatable :: arr(:)
    integer :: i
    allocate(arr(2))
    arr = [1, 2]
    i = 0
    do while (f() > i)
        i = i + 1
    end do
    if (i /= 2) error stop 1
    if (.not. allocated(arr)) error stop 2
    if (any(arr /= [1, 2])) error stop 3
    print *, i, arr
contains
    integer function f()
        f = size(arr)
    end function
end program
