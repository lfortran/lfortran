program gpu_kernel_source_38
    implicit none
    integer :: out(4), i, n

    n = 4
    do concurrent (i=1:4)
        block
            associate (k => n)
                block
                    integer :: work(k)
                    work = 2
                    out(i) = sum(work)
                end block
            end associate
        end block
    end do
    if (any(out /= 8)) error stop 1
    print *, "ok"
end program
