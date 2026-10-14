! Derived types declared in procedures of a template: each instantiated
! procedure has its own instance of the type, also when two procedures
! declare types of the same name. A component may be of another local type,
! and a component bound may use a named constant of the procedure. A local
! type hides a type of the template of the same name only in its procedure,
! whatever the order of instantiation. A structure constructor constructs
! the instantiated type.

module template_local_type_01_m
    implicit none

    template tm {n}
        deferred integer, parameter :: n

        type :: loc
            real :: z
            integer :: b
        end type
    contains
        integer function f() result(r)
            type :: loc
                integer :: b
            end type
            type(loc) :: s
            s%b = n
            associate (q => s)
                r = q%b
            end associate
        end function

        integer function g() result(r)
            integer, parameter :: kk = n*2
            type :: loc
                integer :: b(kk)
                real :: x = 1.5
            end type
            type(loc) :: s, t(2)
            s%b = n
            t(1)%b = 1
            t(2)%b = 2
            r = sum(s%b) + sum(t(1)%b) + sum(t(2)%b) + size(s%b)
            if (s%x /= 1.5) error stop
        end function

        integer function h() result(r)
            type :: inner
                integer :: v = 4
            end type
            type :: loc
                type(inner) :: i
            end type
            type(loc), allocatable :: arr(:)
            allocate(arr(n))
            arr(2)%i%v = 7
            r = size(arr) + arr(1)%i%v + arr(2)%i%v
        end function

        integer function k() result(r)
            type(loc) :: s
            s%z = 2.5
            s%b = 10*n
            associate (q => s)
                r = q%b + int(q%z)
            end associate
        end function

        integer function c() result(r)
            integer :: i
            type :: loc
                integer :: a(n)
                integer :: e = 1
            end type
            type(loc) :: s
            s = loc([(i, i = 1, n)], 5)
            r = sum(s%a) + s%e
            s = loc(s%a + 1)
            r = r + sum(s%a) + s%e
        end function
    end template
end module

program template_local_type_01
    use template_local_type_01_m
    implicit none
    instantiate tm {3}, only: f3 => f, g3 => g, h3 => h, k3 => k, c3 => c
    instantiate tm {2}, only: k2 => k, f2 => f, g2 => g, h2 => h, c2 => c

    print *, f3(), g3(), h3(), k3(), f2(), g2(), h2(), k2()
    if (f3() /= 3) error stop
    if (g3() /= 18 + 6 + 12 + 6) error stop
    if (f2() /= 2) error stop
    if (g2() /= 8 + 4 + 8 + 4) error stop
    if (h3() /= 14) error stop
    if (h2() /= 13) error stop
    if (k3() /= 32) error stop
    if (k2() /= 22) error stop
    print *, c3(), c2()
    if (c3() /= 6 + 5 + 9 + 1) error stop
    if (c2() /= 3 + 5 + 5 + 1) error stop
end program
