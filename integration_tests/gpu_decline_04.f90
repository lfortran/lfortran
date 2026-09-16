! gpu decline reason: RecursiveDeviceFunction
! the loop body calls a pair of mutually recursive procedures, and the device
! call graph cannot be closed over a cycle.
program gpu_decline_04
    implicit none
    integer :: a(4), i
    do concurrent (i = 1:4)
        a(i) = f(i)
    end do
    if (a(1) /= 1) error stop "bad a(1)"
    if (a(2) /= 1) error stop "bad a(2)"
    if (a(3) /= 2) error stop "bad a(3)"
    if (a(4) /= 2) error stop "bad a(4)"
contains
    pure recursive function f(n) result(r)
        integer, intent(in) :: n
        integer :: r
        if (n <= 0) then
            r = 0
        else
            r = 1 + g(n - 2)
        end if
    end function
    pure recursive function g(n) result(r)
        integer, intent(in) :: n
        integer :: r
        if (n <= 0) then
            r = 0
        else
            r = 1 + f(n - 2)
        end if
    end function
end program
