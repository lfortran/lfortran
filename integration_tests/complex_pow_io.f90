program mandelbrot
    implicit none
    integer, parameter :: dp = kind(0.d0)
    real(dp), parameter :: D = 0.035_dp
    integer :: ix, iy, k
    complex(dp) :: c, z
    logical :: escaped

    do iy = -15, 15
        do ix = -35, 35
            c = cmplx(-0.75_dp + D*ix, -1.66_dp * D*iy, dp)
            z = (0.0_dp, 0.0_dp)
            escaped = .false.
            
            do k = 1, 200
                z = z**2 + c
                
                if (z%re**2 + z%im**2 > 4.0_dp) then
                    escaped = .true.
                    exit
                end if
            end do
            
            if (escaped) then
                write(*, "(a)", advance="no") " "
            else
                write(*, "(a)", advance="no") "*"
            end if
        end do
        
        write(*, *) 
    end do
end program mandelbrot