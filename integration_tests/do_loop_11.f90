program do_loop_11
    implicit none
    call sum_up()
    call sum_down()
contains
    subroutine sum_up()
        real :: x, s
        integer :: n
        n = 0
        s = 0.0
        do x = 0.0, 1.0, 0.25
            n = n + 1
            s = s + x
        end do
        print *, n, s
        if (n /= 5) error stop
        if (abs(s - 2.5) > 1e-6) error stop
    end subroutine
    subroutine sum_down()
        real :: x, s
        integer :: n
        n = 0
        s = 0.0
        do x = 1.0, 0.0, -0.5
            n = n + 1
            s = s + x
        end do
        print *, n, s
        if (n /= 3) error stop
        if (abs(s - 1.5) > 1e-6) error stop
    end subroutine
end program
