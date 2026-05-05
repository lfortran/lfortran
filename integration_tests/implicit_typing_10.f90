function func(x, y)
    integer :: x, y
    func = x * y
end function func

program implicit_typing_10
    integer, parameter :: n = 5, m = 5
    dimension a(n), b(m)

    a = [1.0, 2.0, 3.0, 4.0, 5.0]
    b = [1.0, 2.0, 3.0, 4.0, 5.0]

    if (any(a /= [1.0, 2.0, 3.0, 4.0, 5.0])) error stop 1
    if (any(b /= [1.0, 2.0, 3.0, 4.0, 5.0])) error stop 2
    c = func(2, 3)
    if (c /= 6) error stop 3
end program implicit_typing_10