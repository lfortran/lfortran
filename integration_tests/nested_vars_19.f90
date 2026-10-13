! A statement that calls an internal procedure and also refers to an allocatable
! array of the host which that procedure uses, e.g. `arr(1) = f()` (#13601).
program nested_vars_19
    implicit none
    integer, allocatable :: arr(:)
    allocate(arr(2))
    arr(1) = f()
    if (arr(1) /= 2) error stop 1
    if (f() + arr(1) /= 4) error stop 2
    print *, arr(1)
contains
    integer function f()
        f = size(arr)
    end function
end program
