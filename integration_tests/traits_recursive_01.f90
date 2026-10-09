program traits_recursive_01
    use traits_recursive_01_m
    implicit none
    type(Box) :: x
    type(OffsetBox) :: y
    integer :: n
    x = Box(11)
    y = OffsetBox(11)
    print *, first(x, 0), first(x, 1), first(x, 2), first(x, 3), first(x, 4)
    print *, second(x, 0), second(x, 1), second(x, 2), second(x, 3), second(x, 4)
    do n = 0, 8
        if (first(x, n) /= 11+n) error stop 1
        if (second{Box}(x, n) /= 11+n) error stop 2
        if (first{OffsetBox}(y, n) /= 111+n) error stop 3
        if (second(y, n) /= 111+n) error stop 4
        if (total(x, n) /= 11+n) error stop 5
        if (total(y, n) /= 111+n) error stop 6
    end do
    call empty(x)
end program
