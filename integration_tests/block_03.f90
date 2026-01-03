program block_02
    integer :: a
    a = 10
    1 loop: block
        integer :: b
        a = a + 5
        if (a == 15) go to 1
        b = a / 2
        call square(b)
    end block loop
end program block_02

subroutine square(b)
    integer :: b, result
    result = b * b
    if (result /= 100) error stop
    print *, result
end subroutine square
