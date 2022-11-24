
program arrays_15

integer :: b(2)

b = [244, 1404]

call foo()

contains

subroutine foo()
    b(1) = 43
call foo2(size(b), b)
end subroutine

subroutine foo2(m, vec)
    integer :: m, vec(m)
    if (m /= 2) error stop
    if (vec(1) /= 43) error stop
    if (vec(2) /= 1404) error stop
end subroutine

end program
