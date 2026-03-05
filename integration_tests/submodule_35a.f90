program submodule_35
    use submodule_35_parent_mod
    use submodule_35_util_mod, only: point_t
    implicit none
    type(point_t) :: p
    p%x = 3.0
    p%y = 4.0
    call scale(p, 2.0)
    if (abs(p%x - 6.0) > 1e-5) error stop
    if (abs(p%y - 8.0) > 1e-5) error stop
    print *, "ok"
end program submodule_35
