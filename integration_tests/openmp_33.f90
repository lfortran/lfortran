program openmp_33
    use omp_lib
    integer, parameter :: Nx = 600, Ny = 450, n_max = 255, dp=kind(0.d0)
    real(dp), parameter :: xcenter = -0.5_dp, ycenter = 0.0_dp, &
        width = 4, height = 3, dx_di = width/Nx, dy_dj = -height/Ny, &
        x_offset = xcenter - (Nx+1)*dx_di/2, y_offset = ycenter - (Ny+1)*dy_dj/2
    real(dp) :: x, y, x_0, y_0, x_sqr, y_sqr, wtime
    integer :: i, j, n, image(Nx, Ny)
    call omp_set_num_threads(4)
    wtime = omp_get_wtime()
    !$omp parallel shared(image) private(i, j, x, y, x_0, y_0, x_sqr, y_sqr, n)
    !$omp do
    do j = 1, Ny
        y_0 = y_offset + dy_dj * j
        do i = 1, Nx
            x_0 = x_offset + dx_di * i
            x = 0; y = 0; n = 0
            do
                x_sqr = x ** 2; y_sqr = y ** 2
                if (x_sqr + y_sqr > 4 .or. n == n_max) then
                    image(i,j) = 255-n
                    exit
                end if
                y = y_0 + 2 * x * y
                x = x_0 + x_sqr - y_sqr
                n = n + 1
            end do
        end do
    end do
    !$omp end do
    !$omp end parallel
    wtime = omp_get_wtime() - wtime
    print *, 'Time = ', wtime, "(s)"
    print *, sum(image)
    ! if ( sum(image) /= 59157126 ) error stop
end program
