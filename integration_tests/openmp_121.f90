module openmp_121_m
implicit none
contains
    pure subroutine p(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine p

    pure integer function f(x)
        integer, intent(in) :: x
        f = x + 10
    end function f
end module openmp_121_m

pure subroutine q(x)
    integer, intent(inout) :: x
    x = x + 2
end subroutine q

pure integer function g(x)
    integer, intent(in) :: x
    g = x * 3
end function g

subroutine run_calls()
    use openmp_121_m
    implicit none
    integer :: i
    integer :: a(4), b(4), c(4), d(4)
    interface
        pure subroutine q(x)
            integer, intent(inout) :: x
        end subroutine q
        pure integer function g(x)
            integer, intent(in) :: x
        end function g
    end interface

    a = 0
    do concurrent (i = 1:4)
        call p(a(i))
    end do

    b = [1, 2, 3, 4]
    do concurrent (i = 1:4)
        b(i) = f(b(i))
    end do

    c = 0
    do concurrent (i = 1:4)
        call q(c(i))
    end do

    d = [1, 2, 3, 4]
    do concurrent (i = 1:4)
        d(i) = g(d(i))
    end do

    do i = 1, 4
        if (a(i) /= 1) error stop 1
        if (b(i) /= i + 10) error stop 2
        if (c(i) /= 2) error stop 3
        if (d(i) /= i * 3) error stop 4
    end do
end subroutine run_calls

program openmp_121
    call run_calls()
end program openmp_121
