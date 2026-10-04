program template_host_var_07
    use template_host_var_07_user, only: ifill, rfill, n
    implicit none
    integer :: a(3)
    real :: r(4)
    call ifill(a, 4)
    if (any(a /= 4)) error stop 1
    n = 4
    r = 0
    call rfill(r, 2.5)
    if (any(r /= 2.5)) error stop 2
    print *, "ok"
end program
