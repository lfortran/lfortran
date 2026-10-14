module template_deferred_const_14_m
    implicit none
    template tm {n}
        deferred integer, parameter :: n
        integer, parameter :: k = n*2
        type :: w
            integer :: b1(k)
            integer :: b2(k + n)
        end type
        ! The bounds are the template's constants, not these components.
        type :: v
            real :: k
            real :: n
            integer :: z1(k)
            integer :: z2(n)
        end type
    contains
        integer function total() result(r)
            type(w) :: s
            r = size(s%b1) + size(s%b2) + k
        end function
    end template
end module

program template_deferred_const_14
    use template_deferred_const_14_m
    implicit none
    instantiate tm {3}, only: w3 => w, v3 => v
    instantiate tm {2}, only: total2 => total
    type(w3) :: s
    type(v3) :: t
    integer :: k
    k = 42
    s%b1 = 1
    s%b2 = 2
    if (size(s%b1) /= 6) error stop
    if (size(s%b2) /= 9) error stop
    if (sum(s%b1) + sum(s%b2) /= 24) error stop
    t%k = 1.5
    t%n = 2.5
    t%z1 = 3
    t%z2 = 4
    if (size(t%z1) /= 6) error stop
    if (size(t%z2) /= 3) error stop
    if (t%k /= 1.5 .or. t%n /= 2.5) error stop
    if (sum(t%z1) + sum(t%z2) /= 30) error stop
    if (total2() /= 14) error stop
    if (k /= 42) error stop
    print *, size(s%b1), size(s%b2), t%k, t%n, total2()
end program
