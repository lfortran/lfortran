! Module entities and a parent component named after a type accessed through
! a module entity (D5), read from the .mod files of modules compiled
! separately (namespace_modules_33_mod.f90).
program namespace_modules_33
    use namespace_modules_33_mod_b
    use, namespace :: b => namespace_modules_33_mod_b
    implicit none
    type(u) :: v, w
    type(b%a%t) :: s
    type(a%t) :: s2
    real(b%env%real64) :: r
    integer(env%int8) :: k

    ! The module entity `a` of module b, through `use b` and as `b%a`
    if (a%x /= 1) error stop
    if (b%a%x /= 1) error stop
    a%x = 2
    if (b%a%x /= 2) error stop
    if (a%f(3) /= 30) error stop
    if (b%a%f(4) /= 40) error stop

    ! Types through the module entity
    s = b%a%t(6)
    s2 = a%t(x=8)
    if (s%x /= 6 .or. s%twice() /= 12) error stop
    if (s2%x /= 8) error stop

    ! Kinds of an intrinsic module through a module entity
    r = b%half
    k = int(5, env%int8)
    if (kind(r) /= 8 .or. abs(r - real(0.5, b%env%real64)) > 1e-12) error stop
    if (kind(k) /= 1 .or. k /= 5) error stop

    ! The parent component `t` of type u, which extends a%t (D5)
    v%y = 3
    v%t%x = 5
    if (v%x /= 5 .or. v%t%x /= 5) error stop
    if (v%t%twice() /= 10 .or. v%twice() /= 10) error stop
    call set_parent(v, 9)
    if (v%x /= 9 .or. v%y /= 3) error stop
    w = u(t=b%a%t(7), y=4)
    if (w%t%x /= 7 .or. w%x /= 7 .or. w%y /= 4) error stop
    s = v%t
    if (s%x /= 9) error stop

    print *, a%x, s%x, v%t%x, w%t%x, w%y, r, k
end program
