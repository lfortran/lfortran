! Under --fast, a loop of device code that copies one array into another
! stays a loop for the device code generators.
program gpu_do_loop_03
    implicit none
    integer, parameter :: n = 64
    real :: x(16, n), y(16, n)
    integer :: i, j

    y = 2
    do concurrent (i = 1:n)
        block
            integer :: k
            real :: a(16), b(16)
            do k = 1, 16
                b(k) = y(k, i) + i
            end do
            do k = 1, 16
                a(k) = b(k)
            end do
            do k = 1, 16
                x(k, i) = a(k)
            end do
        end block
    end do

    do i = 1, n
        do j = 1, 16
            if (abs(x(j, i) - (2 + i)) > 1e-6) error stop
        end do
    end do
    print *, "ok"
end program
