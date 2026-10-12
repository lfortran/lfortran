! A scalar initializer of an array whose extents or value depend on a
! deferred constant of the template: a component default initializer, or a
! named constant. Each instantiation gets the array of the scalar value, as
! for the same declaration outside a template. A component default
! initializer can also be an expression of a deferred constant.

module template_struct_default_01_m
    implicit none

    template tm {n}
        deferred integer, parameter :: n

        integer, parameter :: a(n) = 1
        integer, parameter :: c(n) = n

        type :: w
            integer :: b(n) = 1
            integer :: c(n) = n
            integer :: d(3) = n
            integer :: e(n, 2) = 5
            real :: r(n) = n
            real :: s(n+1) = 1.5
            character(len=2) :: h(n) = 'ab'
            logical :: l(n) = .true.
            integer :: k = 2*n + 1
            integer :: g(n) = 2*n + 1
        end type
    contains
        integer function f() result(r)
            r = sum(a) + sum(c)
        end function
    end template
end module

program template_struct_default_01
    use template_struct_default_01_m
    implicit none
    instantiate tm {3}, only: w3 => w, f3 => f
    instantiate tm {2}, only: w2 => w, f2 => f
    type(w3) :: s
    type(w2) :: t

    print *, s%b, s%c, s%d, s%e, s%r, s%s, s%h, s%l, s%k, s%g
    if (size(s%b) /= 3 .or. any(s%b /= 1)) error stop
    if (size(s%c) /= 3 .or. any(s%c /= 3)) error stop
    if (size(s%d) /= 3 .or. any(s%d /= 3)) error stop
    if (size(s%e) /= 6 .or. any(s%e /= 5)) error stop
    if (size(s%r) /= 3 .or. any(s%r /= 3.0)) error stop
    if (size(s%s) /= 4 .or. any(s%s /= 1.5)) error stop
    if (size(s%h) /= 3 .or. any(s%h /= 'ab')) error stop
    if (size(s%l) /= 3 .or. .not. all(s%l)) error stop
    if (s%k /= 7) error stop
    if (size(s%g) /= 3 .or. any(s%g /= 7)) error stop

    print *, t%b, t%c, t%d, t%e, t%k, t%g
    if (size(t%b) /= 2 .or. any(t%b /= 1)) error stop
    if (size(t%c) /= 2 .or. any(t%c /= 2)) error stop
    if (size(t%d) /= 3 .or. any(t%d /= 2)) error stop
    if (size(t%e) /= 4 .or. any(t%e /= 5)) error stop
    if (t%k /= 5) error stop
    if (size(t%g) /= 2 .or. any(t%g /= 5)) error stop

    print *, f3(), f2()
    if (f3() /= 12) error stop
    if (f2() /= 6) error stop
end program
