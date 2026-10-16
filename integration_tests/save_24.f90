module save_24_t
implicit none
type :: tt
    integer :: v = 5
    integer, allocatable :: a(:)
end type
end module

module save_24_ma
use save_24_t, only: tt
implicit none
contains
    integer function f()
        integer, save :: s = 0
        integer, save :: t(2) = 0
        type(tt), save :: x
        s = s + 1
        t = t + 1
        x%v = x%v + 1
        f = s + t(2) + 100*x%v
    end function

    integer function h1()
        h1 = g()
    contains
        integer function g()
            integer, save :: s = 0
            s = s + 1
            g = s
        end function
    end function

    integer function h2()
        h2 = g()
    contains
        integer function g()
            integer, save :: s = 0
            s = s + 100
            g = s
        end function
    end function
end module

module save_24_mb
use save_24_t, only: tt
implicit none
contains
    integer function f()
        integer, save :: s = 0
        integer, save :: t(2) = 0
        type(tt), save :: x
        s = s + 10
        t = t + 10
        x%v = x%v + 10
        f = s + t(2) + 100*x%v
    end function
end module

program save_24
use save_24_ma, only: fa => f, h1, h2
use save_24_mb, only: fb => f
implicit none
if (fa() /= 602) error stop
if (fb() /= 1520) error stop
if (fa() /= 704) error stop
if (fb() /= 2540) error stop
if (f() /= 1000) error stop
if (h1() /= 1) error stop
if (h2() /= 100) error stop
if (h1() /= 2) error stop
if (h2() /= 200) error stop
if (f() /= 2000) error stop
print *, "ok"
contains
    integer function f()
        integer, save :: s = 0
        s = s + 1000
        f = s
    end function
end program
