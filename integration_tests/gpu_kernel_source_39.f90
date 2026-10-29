program gpu_kernel_source_39
    implicit none
    integer :: out(4), i, n
    n = 4
    do concurrent (i=1:4)
        out(i) = f(n)
    end do
    if (any(out /= 20)) error stop 1
    if (f(3) /= 15) error stop 2
    if (f(0) /= 0) error stop 3
    print *, "ok"
contains
    pure integer function f(m) result(r)
        integer, intent(in) :: m
        integer :: head(m)
        head = 2
        associate (k => m)
            block
                integer :: tail(k)
                tail = 3
                r = sum(head) + sum(tail)
            end block
        end associate
    end function
end program
