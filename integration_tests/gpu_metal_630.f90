program gpu_metal_630
    ! Scalar transfer between integer(4) and real(4) in a loop offloaded to
    ! the GPU: a variable, an expression and a constant as the source, in
    ! both directions (lfortran/lfortran#16613).
    use iso_fortran_env, only: int32, real32
    implicit none
    integer(int32) :: input(6), output(6), shifted(6)
    real(real32) :: values(6), scaled(6), negpi(6)
    integer :: i

    input = [0_int32, int(z'80000000', int32), int(z'3F800000', int32), &
        int(z'BFC00000', int32), int(z'40800000', int32), int(z'C0800000', int32)]
    output = 1_int32
    values = 2.0_real32
    shifted = 0_int32
    scaled = 0.0_real32
    negpi = 0.0_real32
    do concurrent (i = 1:6)
        values(i) = transfer(input(i), 0.0_real32)
        output(i) = transfer(values(i), 0_int32)
        scaled(i) = 2.0_real32 * transfer(input(i) + 0_int32, 1.0_real32)
        shifted(i) = transfer(2.0_real32 * values(i), 0_int32)
        negpi(i) = transfer(int(z'C0490FDB', int32), 0.0_real32) * real(i, real32)
    end do
    if (storage_size(values) /= 32 .or. storage_size(input) /= 32) error stop 1
    if (any(output /= input)) error stop 2
    if (any(values /= [0.0_real32, 0.0_real32, 1.0_real32, -1.5_real32, &
        4.0_real32, -4.0_real32])) error stop 3
    if (.not. btest(transfer(values(2), 0_int32), 31)) error stop 4
    if (btest(transfer(values(1), 0_int32), 31)) error stop 5
    if (any(scaled /= 2.0_real32 * values)) error stop 6
    if (shifted(3) /= int(z'40000000', int32)) error stop 7
    if (shifted(5) /= int(z'41000000', int32)) error stop 8
    if (shifted(1) /= 0_int32) error stop 9
    if (abs(negpi(1) + 3.14159274_real32) > 1.0e-6_real32) error stop 10
    if (abs(negpi(6) - 6.0_real32 * negpi(1)) > 1.0e-5_real32) error stop 11
    print '(a,6(1x,z8.8))', 'gpu_metal_630: PASS', output
end program
