module gpu_metal_546_mod
    implicit none
contains
    integer function set_to_three(k)
        integer, intent(inout) :: k
        k = 3
        set_to_three = 0
    end function
end module

program gpu_metal_546
    ! Statements of the time loop change the producer's bound `m` other
    ! than by assigning it: an impure function through its INTENT(INOUT)
    ! argument, and an IOSTAT= specifier. The first time step writes more
    ! elements of f than the last, which the program sees afterwards.
    use gpu_metal_546_mod
    implicit none
    real, allocatable :: q(:), f(:)
    real :: expected_f(8)
    character(len=8) :: str
    integer :: n, s, i, m, dummy
    n = 8
    allocate(q(n), f(n))

    f = -1.0
    q = 0.0
    m = 7
    do s = 1, 3
        do concurrent (i = 1:m)
            f(i) = q(i) + real(i)
        end do
        do concurrent (i = 2:m)
            q(i) = q(i) + f(i) - f(i - 1)
        end do
        dummy = set_to_three(m)
    end do
    expected_f = [1.0, 5.0, 5.0, 4.0, 5.0, 6.0, 7.0, -1.0]
    print *, f
    if (dummy /= 0 .or. m /= 3) error stop
    if (any(abs(f - expected_f) > 1e-5)) error stop

    f = -1.0
    q = 0.0
    m = 7
    do s = 1, 3
        do concurrent (i = 1:m)
            f(i) = q(i) + real(i)
        end do
        do concurrent (i = 2:m)
            q(i) = q(i) + f(i) - f(i - 1)
        end do
        write(str, '(i0)', iostat=m) s
    end do
    expected_f = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, -1.0]
    print *, f
    if (m /= 0 .or. trim(str) /= '3') error stop
    if (any(abs(f - expected_f) > 1e-5)) error stop
end program
