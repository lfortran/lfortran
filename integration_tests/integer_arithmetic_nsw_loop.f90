program integer_arithmetic_nsw_loop
    implicit none
    integer :: n, i, a(3)
    character(5) :: text
    do n = 0, 3
        text = make_text(n)
        a = -1
        a(1:n) = [(120+i, i=1,n)]
        if (any(a(1:n) /= [(120+i, i=1,n)])) error stop
        if (any(a(n+1:) /= -1)) error stop
    end do
    if (text /= "xxx  ") error stop
contains
    function make_text(n) result(text)
        integer, intent(in) :: n
        character(5) :: text
        text = repeat('x', n)
    end function
end program
