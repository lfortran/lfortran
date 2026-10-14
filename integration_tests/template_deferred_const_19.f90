module template_deferred_const_19_m
    implicit none
    template tm {n}
        deferred integer, parameter :: n
        integer, parameter :: k = n*2
        integer, parameter :: m = k + 1
        integer, parameter :: arr(2) = [n, k]
        type :: w
            integer :: b(m)
        end type
    contains
        integer function f()
            f = m*10
        end function
    end template

    template t2 {p}
        deferred integer, parameter :: p
        integer, parameter :: c = p*2
    contains
        integer function f2()
            f2 = p + 1
        end function
    end template

    template tn {n}
        deferred integer, parameter :: n
        instantiate t2 {n}
    contains
        integer function g()
            g = f2() + c
        end function
    end template

    template tp {n}
        deferred integer, parameter :: n
    contains
        integer function h()
            instantiate t2 {n}
            h = f2() + c
        end function
    end template
end module

module template_deferred_const_19_n
    use template_deferred_const_19_m
    implicit none
    instantiate tm {5}
end module

program template_deferred_const_19
    use template_deferred_const_19_m
    use template_deferred_const_19_n, only: k5 => k, m5 => m
    implicit none
    ! Without an only-list, the named constants of the template are
    ! accessible, like its procedures and derived types.
    instantiate tm {3}
    ! A template instantiated without an only-list inside another template
    ! has named constants that depend on a deferred constant.
    instantiate tn {3}, only: g3 => g
    instantiate tp {4}, only: h4 => h
    type(w) :: s
    integer :: x(k)
    print *, k, m, arr, size(s%b), f(), size(x), k5, m5, g3(), h4()
    if (k /= 6) error stop
    if (m /= 7) error stop
    if (any(arr /= [3, 6])) error stop
    if (size(s%b) /= 7) error stop
    if (f() /= 70) error stop
    if (size(x) /= 6) error stop
    if (k5 /= 10 .or. m5 /= 11) error stop
    if (g3() /= 10) error stop
    if (h4() /= 13) error stop
end program
