program gpu_metal_631
    ! Scalar transfer between integer(8) and real(8) in a loop offloaded to
    ! the GPU, in both directions (lfortran/lfortran#16613).
    use iso_fortran_env, only: int64, real64
    implicit none
    integer(int64) :: input(4), output(4)
    real(real64) :: values(4)
    integer :: i

    input = [0_int64, int(z'3FF0000000000000', int64), &
        int(z'C004000000000000', int64), int(z'4010000000000000', int64)]
    output = 1_int64
    values = 2.0_real64
    do concurrent (i = 1:4)
        values(i) = transfer(input(i), 0.0_real64)
        output(i) = transfer(values(i), 0_int64)
    end do
    if (storage_size(values) /= 64 .or. storage_size(input) /= 64) error stop 1
    if (any(output /= input)) error stop 2
    if (any(values /= [0.0_real64, 1.0_real64, -2.5_real64, 4.0_real64])) error stop 3
    print '(a,4(1x,z16.16))', 'gpu_metal_631: PASS', output
end program
