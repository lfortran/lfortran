program gpu_metal_330
    implicit none
    integer :: i
    real(8) :: values(4)
    do concurrent (i = 1:4)
        values(i) = real(i, 8) / 3.0_8
    end do
    if (abs(sum(values) - 10.0_8/3.0_8) > 1.e-14_8) error stop 1
end program
