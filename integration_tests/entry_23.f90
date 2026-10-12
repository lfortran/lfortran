module entry_23_mod
    implicit none
contains
    function f(i) result(r)
        integer, intent(in) :: i
        integer :: r
        r = i + 3
        return
    entry g(i) result(r)
        r = i * 3
    end function

    function total(n) result(r)
        integer, intent(in) :: n
        integer :: r, s, k
        r = 0
        do k = 1, n
            r = r + k
        end do
        return
    entry factorial(n) result(s)
        s = 1
        do k = 1, n
            s = s * k
        end do
    end function

    function sumsq(n)
        integer, intent(in) :: n
        integer :: sumsq, sumcube, k
        sumsq = 0
        do k = 1, n
            sumsq = sumsq + k**2
        end do
        return
    entry sumcube(n)
        sumcube = 0
        do k = 1, n
            sumcube = sumcube + k**3
        end do
    end function

    function shifted(i) result(r)
        integer, intent(in) :: i
        integer :: r
        associate (q => i + 3)
            r = q
        end associate
        return
    entry scaled(i) result(r)
        block
            integer :: t
            t = i * 3
            r = t
        end block
    end function
end module

function fe(i) result(r)
    integer, intent(in) :: i
    integer :: r
    r = i + 3
    return
entry ge(i) result(r)
    r = i * 3
end function

program entry_23
    use entry_23_mod
    implicit none
    interface
        function fe(i) result(r)
            integer, intent(in) :: i
            integer :: r
        end function
        function ge(i) result(r)
            integer, intent(in) :: i
            integer :: r
        end function
    end interface
    print *, f(1), g(2)
    if (f(1) /= 4) error stop
    if (g(2) /= 6) error stop
    print *, total(4), factorial(4)
    if (total(4) /= 10) error stop
    if (factorial(4) /= 24) error stop
    print *, sumsq(3), sumcube(3)
    if (sumsq(3) /= 14) error stop
    if (sumcube(3) /= 36) error stop
    print *, shifted(1), scaled(2)
    if (shifted(1) /= 4) error stop
    if (scaled(2) /= 6) error stop
    print *, fe(1), ge(2)
    if (fe(1) /= 4) error stop
    if (ge(2) /= 6) error stop
end program
