program character_26
    ! Test that character array element assignments in a loop
    ! do not cause stack overflow from alloca in loop body
    implicit none
    integer, parameter :: n = 100000
    character(len=1) :: a(n), b(n)
    integer :: i
    a = "x"
    b = "y"
    do i = 1, n
        a(i) = b(i)
        b(i) = a(i)
    end do
    if (a(1) /= "y") error stop
    if (b(1) /= "y") error stop
    if (a(n) /= "y") error stop
    if (b(n) /= "y") error stop
    print *, "ok"
end program
