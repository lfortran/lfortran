module nested_constructor_controls
    implicit none
    integer :: starts, ends, steps, inner_calls
contains
    function first(n) result(a)
        integer, intent(in) :: n
        integer, allocatable :: a(:)
        starts = starts + 1
        allocate(a(n))
        a = 1
    end function
    integer function last()
        ends = ends + 1
        last = 1
    end function
    integer function stride()
        steps = steps + 1
        stride = 1
    end function
    integer function inner_limit(i)
        integer, intent(in) :: i
        inner_calls = inner_calls + 1
        inner_limit = i-1
    end function
end module

program arrays_162
    use nested_constructor_controls
    implicit none
    integer :: n, one, i, j, h, k, a(3), b(3), empty(0)
    n = 3
    one = 1
    i = 19
    j = 23
    h = 29
    starts = 0
    ends = 0
    steps = 0
    inner_calls = 0
    do k = 1, 2
        a = [(i, i=int(sum([(first(n),j=1,one)]),8), &
            int(size([(j,j=1,last())]),2), &
            -int(size([(j,j=1,stride())]),8))]
        if (any(a /= [3,2,1])) error stop 1
        if (starts /= k .or. ends /= k .or. steps /= k) error stop 2
        b = [((10*i+j,j=1,size([(h,h=1,inner_limit(i))])),i=1,n)]
        if (any(b /= [21,31,32])) error stop 3
        if (inner_calls /= 3*k) error stop 4
    end do
    empty = [(i, i=0,size([(j,j=1,last())]), &
        -int(size([(j,j=1,stride())]),8))]
    if (size(empty) /= 0) error stop 5
    if (starts /= 2 .or. ends /= 3 .or. steps /= 3) error stop 6
    if (inner_calls /= 6) error stop 7
    if (i /= 19 .or. j /= 23 .or. h /= 29) error stop 8
end program
