program string_97
    implicit none
    character(len=1), dimension(3) :: b

    print "(3(1X,A))", transfer('abc', ['x'])

    b = transfer('abc', b)

    if (any(b /= ['a','b','c'])) error stop 'Test failed'
end program string_97
