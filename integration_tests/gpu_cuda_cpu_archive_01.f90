program gpu_cuda_cpu_archive_01
    use gpu_cuda_cpu_archive_01_m, only: twice
    implicit none
    real :: x(4), y(4)
    x = [1.0, 2.0, 3.0, 4.0]
    y = -1.0
    call twice(x, y)
    if (any(y /= [2.0, 4.0, 6.0, 8.0])) error stop 1
    print *, "ok"
end program
