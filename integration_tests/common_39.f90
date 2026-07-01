program common_39
    implicit none
    call test()
end program

subroutine test()
    integer(4) :: n
    real :: arr
    parameter(n = 0)
    common /cname/ arr(n)
    if (n /= 0) error stop
end subroutine test
