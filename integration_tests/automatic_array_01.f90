module automatic_array_01_mod
    ! The bounds of an automatic array are evaluated on entry. Redefining a
    ! variable used in them later must not change the array.
    implicit none
    integer :: g = 3

contains

    pure integer function f(n)
        integer, intent(in) :: n
        f = n
    end function

    subroutine module_procedure()
        real :: a(g)
        g = 7
        a = 1
        if (size(a) /= 3) error stop "module_procedure: size"
        if (abs(sum(a) - 3) > 1e-6) error stop "module_procedure: sum"
        g = 3
    end subroutine

    subroutine host_association()
        ! An internal procedure uses the automatic array of its host.
        real :: a(g, g + 1)
        g = 1
        call fill()
        if (size(a, 1) /= 3 .or. size(a, 2) /= 4) error stop "host_association: size"
        if (abs(a(3, 4) - 43) > 1e-6) error stop "host_association: a(3, 4)"
        g = 3
    contains
        subroutine fill()
            integer :: i, j
            do j = 1, size(a, 2)
                do i = 1, size(a, 1)
                    a(i, j) = i + 10*j
                end do
            end do
        end subroutine
    end subroutine

    recursive subroutine recursion(n)
        integer, value :: n
        real :: a(n)
        a = n
        if (n > 1) call recursion(n - 1)
        if (size(a) /= n) error stop "recursion: size"
        if (abs(sum(a) - n*n) > 1e-6) error stop "recursion: sum"
    end subroutine

    subroutine show(k, expected)
        integer, intent(in) :: k, expected
        if (k /= expected) error stop "show: wrong actual argument"
    end subroutine

    subroutine dummy_bound(n)
        integer, intent(inout) :: n
        real :: a(n)
        n = 7
        call show(n, 7)
        a = 2
        if (size(a) /= 3) error stop "dummy_bound: size"
        if (abs(sum(a) - 6) > 1e-6) error stop "dummy_bound: sum"
    end subroutine

    ! Host code, also under --gpu: a PURE procedure can define a variable
    ! used in a bound too.
    pure subroutine pure_dummy_bound(n, s)
        integer, intent(inout) :: n
        real, intent(out) :: s
        real :: a(n)
        n = 7
        a = 1
        s = sum(a) + size(a)
    end subroutine

    pure function pure_block_bound(n0) result(s)
        integer, intent(in) :: n0
        real :: s
        integer :: k
        k = n0
        block
            real :: tmp(k)
            k = 7
            tmp = 2
            s = sum(tmp) + size(tmp)
        end block
    end function

    subroutine assumed_shape(x, n)
        real, intent(in) :: x(:)
        integer, intent(in) :: n
        if (size(x) /= n) error stop "assumed_shape: size"
    end subroutine

end module

program automatic_array_01
    use automatic_array_01_mod
    implicit none
    integer :: n
    real :: s

    call internal()
    call module_procedure()
    call host_association()
    call recursion(4)
    n = 3
    call dummy_bound(n)
    if (n /= 7) error stop "dummy_bound: n"
    n = 3
    call pure_dummy_bound(n, s)
    if (n /= 7) error stop "pure_dummy_bound: n"
    if (abs(s - 6) > 1e-6) error stop "pure_dummy_bound: s"
    s = pure_block_bound(3)
    if (abs(s - 9) > 1e-6) error stop "pure_block_bound: s"
    call external_procedure()
    call blocks()
    if (g /= 3) error stop "g"
    print *, "ok"

contains

    subroutine internal()
        real :: tmp(f(g)), tmp2(g)
        real :: b(g:2*g, -g:g)
        character(len=2) :: c(g)
        integer :: sh(2)
        g = 7
        tmp = 1
        tmp2 = 2
        b = 3
        c = "ab"
        if (size(tmp) /= 3 .or. size(tmp2) /= 3) error stop "internal: size"
        if (abs(sum(tmp) - 3) > 1e-6) error stop "internal: sum(tmp)"
        if (abs(sum(tmp2) - 6) > 1e-6) error stop "internal: sum(tmp2)"
        if (abs(maxval(tmp + tmp2) - 3) > 1e-6) error stop "internal: maxval"
        if (any(shape(tmp) /= [3])) error stop "internal: shape(tmp)"
        if (lbound(b, 1) /= 3 .or. ubound(b, 1) /= 6) error stop "internal: bounds 1"
        if (lbound(b, 2) /= -3 .or. ubound(b, 2) /= 3) error stop "internal: bounds 2"
        sh = shape(b)
        if (sh(1) /= 4 .or. sh(2) /= 7) error stop "internal: shape(b)"
        if (abs(b(3, -3) - 3) > 1e-6 .or. abs(b(6, 3) - 3) > 1e-6) error stop "internal: b"
        if (count(b > 2) /= 28) error stop "internal: count(b)"
        if (size(c) /= 3 .or. c(3) /= "ab") error stop "internal: c"
        call assumed_shape(tmp, 3)
        call assumed_shape(b(:, 0), 4)
        g = 3
    end subroutine

    subroutine blocks()
        integer :: m
        m = 2
        block
            real :: a(m)
            m = 5
            a = 1
            if (size(a) /= 2) error stop "blocks: size"
            if (abs(sum(a) - 2) > 1e-6) error stop "blocks: sum"
        end block
        if (m /= 5) error stop "blocks: m"
    end subroutine

end program

subroutine external_procedure()
    use automatic_array_01_mod, only: g
    implicit none
    real :: a(g)
    g = 7
    a = 1
    if (size(a) /= 3) error stop "external_procedure: size"
    if (abs(sum(a) - 3) > 1e-6) error stop "external_procedure: sum"
    g = 3
end subroutine
