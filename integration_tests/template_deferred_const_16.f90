module template_deferred_const_16_m
    implicit none
    template outer {n}
        deferred integer, parameter :: n
        integer, parameter :: k = n*2
        template inner {p}
            deferred integer, parameter :: p
            integer, parameter :: z = k*10
            integer, parameter :: a = z + k
            ! Have a value only once `inner` is instantiated.
            integer, parameter :: c = p
            integer, parameter :: y = p*10 + k
            type :: w
                integer :: b(p)
                integer :: c(a)
                integer :: d(y)
            end type
        contains
            integer function f() result(r)
                integer, parameter :: e = c*3
                r = e
            end function
        end template
    end template
end module

program template_deferred_const_16
    use template_deferred_const_16_m
    implicit none
    integer, parameter :: k = 100, z = 300
    instantiate outer {3}, only: inner3 => inner
    instantiate inner3 {4}, only: w34 => w, a34 => a, z34 => z, c34 => c, &
        y34 => y, f34 => f
    type(w34) :: s
    if (size(s%b) /= 4 .or. size(s%c) /= 66 .or. size(s%d) /= 46) error stop
    if (a34 /= 66 .or. z34 /= 60) error stop
    if (c34 /= 4 .or. y34 /= 46 .or. f34() /= 12) error stop
    if (k /= 100 .or. z /= 300) error stop
    print *, size(s%b), size(s%c), size(s%d), a34, z34, c34, y34, f34()
end program
