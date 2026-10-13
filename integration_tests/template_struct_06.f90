module template_struct_06_m
    ! Type-bound operators of a derived type declared in a template, used
    ! on an instantiation of the type.
    implicit none

    template tm {t}
        deferred type :: t
        type :: w_t
            type(t) :: v
            integer :: n
        contains
            procedure :: add
            procedure :: scale
            procedure :: assign_int
            generic :: operator(+) => add
            generic :: operator(.m.) => scale
            generic :: assignment(=) => assign_int
        end type
    contains
        function add(this, n) result(r)
            class(w_t), intent(in) :: this
            integer, intent(in) :: n
            type(w_t) :: r
            r%v = this%v
            r%n = this%n + n
        end function

        function scale(this, n) result(r)
            class(w_t), intent(in) :: this
            integer, intent(in) :: n
            type(w_t) :: r
            r%v = this%v
            r%n = this%n * n
        end function

        subroutine assign_int(this, n)
            class(w_t), intent(inout) :: this
            integer, intent(in) :: n
            this%n = n
        end subroutine
    end template
end module

program template_struct_06
    use template_struct_06_m
    implicit none
    instantiate tm {integer}, only: iw => w_t
    instantiate tm {real}, only: rw => w_t
    type(iw) :: w, y
    type(rw) :: r, s

    w%v = 7
    w = 5
    if (w%n /= 5) error stop
    y = w + 2
    if (y%v /= 7 .or. y%n /= 7) error stop
    y = w .m. 3
    if (y%v /= 7 .or. y%n /= 15) error stop
    r%v = 1.5
    r = 4
    s = (r .m. 2) + 1
    if (abs(s%v - 1.5) > 1e-6 .or. s%n /= 9) error stop
    print *, "ok"
end program
