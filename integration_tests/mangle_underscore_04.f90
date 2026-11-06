program mangle_underscore_04
    use mangle_underscore_04_m, only: t, x, drv
    implicit none
    integer :: k
    k = 0
    call t(k)
    call drv(k)
    x = x + 1
    print '(I0, 1X, I0)', k, x
    if (k /= 103) error stop
    if (x /= 8) error stop
end program
