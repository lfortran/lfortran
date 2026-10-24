program gpu_metal_507
    implicit none
    integer, parameter :: n = 8
    integer, target :: a(n)
    integer, pointer :: p(:)
    integer :: i, step

    a = [(i, i = 1, n)]
    p => a
    ! The assignment covers a, but p needs its old values at the launch.
    ! Neither buffer may be restricted, and the runtime must not discard a.
    do step = 1, 3
        do concurrent (i = 1:n)
            a(i) = p(i) + 1
        end do
    end do
    do i = 1, n
        if (a(i) /= i + 3) error stop 1
    end do
    print *, "ok"
end program
