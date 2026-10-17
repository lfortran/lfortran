module template_deferred_const_20_m
    implicit none
    template tm {n}
        deferred integer, parameter :: n
        integer, parameter :: arr(2) = [n, 2*n]
        integer, parameter :: arr3(3) = [1, n, 4*n]
        integer, parameter :: arr0(0:2) = [n, 2*n, 3*n]
        ! Elements of named constant arrays built from a deferred constant
        ! are constant expressions, also as component bounds.
        type :: w
            integer :: b(arr(2))
            integer :: c(arr(1) + 1, arr3(3))
            integer :: d(arr0(n - 1))
        end type
    contains
        integer function f()
            type(w) :: s
            f = size(s%b)*100 + size(s%c)
        end function
    end template
end module

program template_deferred_const_20
    use template_deferred_const_20_m
    implicit none
    instantiate tm {3}, only: w3 => w, f3 => f
    type(w3) :: s
    print *, size(s%b), size(s%c, 1), size(s%c, 2), size(s%d), f3()
    if (size(s%b) /= 6) error stop
    if (size(s%c, 1) /= 4 .or. size(s%c, 2) /= 12) error stop
    if (f3() /= 648) error stop
    if (size(s%d) /= 9) error stop
end program
