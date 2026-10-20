program gpu_kernel_source_11
    implicit none
    integer :: a(4), i
    do concurrent (i = 1:4)
        a(i) = i
    end do
    do i = 1, 4
        if (a(i) /= i) error stop
    end do
    print *, "ok"
end program
