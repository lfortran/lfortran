program do_concurrent_13
    integer, parameter :: Nx = 40, Ny = 30, n_max = 255, dp=kind(0.d0)
    real(dp), parameter :: xcenter = -0.5_dp, ycenter = 0.0_dp, &
        width = 4, height = 3, dx_di = width/Nx, dy_dj = -height/Ny, &
        x_offset = xcenter - (Nx+1)*dx_di/2, y_offset = ycenter - (Ny+1)*dy_dj/2
    real(dp) :: x, y, x_0, y_0, x_sqr, y_sqr, wtime
    integer :: i, j, n, image(Nx, Ny)
    do concurrent (j = 1:Ny) shared(image) local(i, x, y, x_0, y_0, x_sqr, y_sqr, n)
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
    print *, sum(image)
    if ( sum(image) /= 263354 ) error stop
end program
