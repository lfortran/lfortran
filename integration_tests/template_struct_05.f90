module template_struct_05_m
    ! Generic bindings of a derived type declared in a template, and of a
    ! module type declared before the template.
    implicit none

    type :: a_t
        integer :: i
    contains
        procedure :: get_a
        generic :: get => get_a
    end type

    template tm {t}
        deferred type :: t
        type :: w_t
            type(t) :: v
            integer :: n
        contains
            procedure :: get_w
            procedure :: scale
            generic :: get => get_w
            generic :: operator(.m.) => scale
        end type
    contains
        function get_w(this) result(r)
            class(w_t), intent(in) :: this
            type(t) :: r
            r = this%v
        end function

        function scale(this, n) result(r)
            class(w_t), intent(in) :: this
            integer, intent(in) :: n
            type(w_t) :: r
            r%v = this%v
            r%n = this%n * n
        end function

        function pick(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r
            type(w_t) :: w, y
            w%v = x
            w%n = 2
            y = w .m. 3
            if (y%n /= 6) error stop
            r = y%get()
        end function
    end template
contains
    integer function get_a(this)
        class(a_t), intent(in) :: this
        get_a = this%i
    end function
end module

program template_struct_05
    use template_struct_05_m
    implicit none
    instantiate tm {integer}, only: ipick => pick
    instantiate tm {real}, only: rpick => pick
    instantiate tm {integer}, only: iw => w_t
    type(a_t) :: a
    type(iw) :: w, y

    a%i = 5
    if (a%get() /= 5) error stop
    if (ipick(4) /= 4) error stop
    if (abs(rpick(1.5) - 1.5) > 1e-6) error stop
    w%v = 7
    w%n = 5
    y = w%scale(2)
    if (y%get() /= 7) error stop
    if (y%n /= 10) error stop
    print *, "ok"
end program
