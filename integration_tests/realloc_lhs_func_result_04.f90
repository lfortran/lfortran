! Reallocation on assignment of the explicit-shape array result of a
! function that reads the variable it is assigned to, by use or host
! association or through an associate name: the variable is defined only
! after the value is evaluated (F2018 10.2.1.3), so the function sees it
! unchanged. GFortran 15 returns the result into the variable and so sees
! it defined, hence this test is not run with GFortran.
module realloc_lhs_func_result_04_m
    implicit none
    real, allocatable :: g(:)

contains

    function plus_g(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a))
        r = -1
        r = a + g(1)
    end function

    ! The size of `g` before the assignment
    function size_g(n) result(r)
        integer, intent(in) :: n
        real :: r(n)
        r = size(g)
    end function

    function reversed(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a))
        integer :: i
        do i = 1, size(a)
            r(i) = a(size(a) + 1 - i)
        end do
    end function

    subroutine host_target(n)
        integer, intent(in) :: n
        real, allocatable :: h(:)
        allocate(h(n))
        h = 1
        h = plus_h([10., 20., 30.])
        if (size(h) /= 3 .or. any(h /= [11., 21., 31.])) error stop "host"
        h = plus_h([10., 20., 30.])
        if (any(h /= [21., 31., 41.])) error stop "host conforming"
        h = size_h(2)
        if (size(h) /= 2 .or. any(h /= 3.)) error stop "host size"
    contains
        function size_h(m) result(r)
            integer, intent(in) :: m
            real :: r(m)
            r = size(h)
        end function

        function plus_h(a) result(r)
            real, intent(in) :: a(:)
            real :: r(size(a))
            r = -1
            r = a + h(1)
        end function
    end subroutine

end module

program realloc_lhs_func_result_04
    use realloc_lhs_func_result_04_m
    implicit none
    real, allocatable :: c(:)

    g = [100., 200.]
    g = plus_g([1., 2., 3.])
    if (size(g) /= 3 .or. any(g /= [101., 102., 103.])) error stop "module variable"
    g = plus_g([1., 2., 3.])
    if (any(g /= [102., 103., 104.])) error stop "module variable conforming"

    g = size_g(4)
    if (size(g) /= 4 .or. any(g /= 3.)) error stop "module variable size"

    call host_target(5)
    call host_target(3)

    c = [4., 8., 12.]
    associate (b => c)
        c = reversed(b)
    end associate
    if (size(c) /= 3 .or. any(c /= [12., 8., 4.])) error stop "associate"
    associate (b => c)
        c = reversed(b(1:2))
    end associate
    if (size(c) /= 2 .or. any(c /= [8., 12.])) error stop "associate nonconforming"
    print *, g, c
end program
