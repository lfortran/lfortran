program gpu_kernel_source_37
    implicit none
    integer :: out(4), i, n

    n = 4
    do concurrent (i=1:4)
        out(i) = f(n)
    end do
    if (any(out /= 8)) error stop 1
    if (f(3) /= 6) error stop 2
    if (f(0) /= 0) error stop 3

    do concurrent (i=1:4)
        associate (k => n)
            block
                integer :: work(k)
                work = 3
                out(i) = sum(work)
            end block
        end associate
    end do
    if (any(out /= 12)) error stop 4
    print *, "ok"
contains
    pure integer function f(m) result(r)
        integer, intent(in) :: m
        associate (k => m)
            block
                integer :: work(m)
                work = 2
                r = sum(work) + k - m
            end block
        end associate
    end function
end program
