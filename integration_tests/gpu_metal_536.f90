module gpu_metal_536_mod
    implicit none
    integer :: nt, kt
    equivalence (nt, kt)
end module

! A time loop nested in a sequential loop whose end `nt` is in
! EQUIVALENCE with `kt`, which the time loop assigns: the inner loop runs
! 4 iterations the first time and 1 afterwards. Time skewing, which
! counts the iterations of the nest once before it starts, must not
! apply to it.
program gpu_metal_536
    use gpu_metal_536_mod
    implicit none
    integer, parameter :: n = 5003
    real :: u(n), d(n)
    integer :: s, t, k
    u = 0.0
    nt = 4
    do s = 1, 3
        do t = 1, nt
            kt = 1
            do concurrent (k = 2:n)
                d(k) = u(k) + 0.5 * (u(k - 1) - u(k)) + 1.0
            end do
            do concurrent (k = 2:n)
                u(k) = d(k)
            end do
        end do
    end do
    print *, u(n), nt
    if (abs(u(n) - 6.0) > 1e-5) error stop 1
    if (nt /= 1) error stop 2
    print *, "ok"
end program
