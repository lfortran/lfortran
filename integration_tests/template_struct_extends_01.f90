module template_struct_extends_01_m0
    implicit none
    type :: bump_t
        integer :: n = 0
    contains
        procedure :: bump
    end type
contains
    integer function op(x)
        integer, intent(in) :: x
        op = x + 10
    end function
    subroutine bump(self)
        class(bump_t), intent(inout) :: self
        self%n = op(self%n)
    end subroutine
end module

module template_struct_extends_01_m
    use template_struct_extends_01_m0, only: bump_t
    implicit none
    type :: base_t
        integer :: m
    end type
    template tm {t}
        deferred type :: t
        type :: b_t
            integer :: n
            type(t) :: v
        contains
            procedure :: get
        end type
        type, extends(b_t) :: c_t
            integer :: k
        end type
        ! The parent type sorts after the type extending it.
        type :: z_t
            type(t) :: w
        end type
        type, extends(z_t) :: a_t
        end type
        ! The parent type is not the template's.
        type, extends(base_t) :: d_t
            type(t) :: u
        end type
    contains
        function get(self) result(r)
            class(b_t), intent(in) :: self
            type(t) :: r
            r = self%v
        end function
        integer function f(x)
            type(t), intent(in) :: x
            type(c_t) :: c
            c%n = 3
            c%v = x
            c%k = 4
            f = c%n + c%k
        end function
        function g(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r
            type(c_t) :: c
            c%v = x
            r = c%get()
        end function
    end template
    ! The parent type is instantiated as a dependency of `f` and `g` only.
    instantiate tm {integer}, only: f_int => f, g_int => g

    requirement r {t, op}
        deferred type :: t
        deferred interface
            function op(x) result(y)
                type(t), intent(in) :: x
                type(t) :: y
            end function
        end interface
    end requirement
    ! The bound procedure of the parent calls a procedure named like a
    ! template parameter.
    template tr {t, op}
        require :: r {t, op}
        type, extends(bump_t) :: e_t
            type(t) :: v
        end type
    end template

    template ta {t}
        deferred type :: t
        type, abstract :: x_t
        contains
            procedure(iget), deferred :: get
        end type
        abstract interface
            function iget(self) result(r)
                import :: x_t, t
                class(x_t), intent(in) :: self
                type(t) :: r
            end function
        end interface
        type, extends(x_t) :: y_t
            type(t) :: v
        contains
            procedure :: get => yget
        end type
    contains
        function yget(self) result(r)
            class(y_t), intent(in) :: self
            type(t) :: r
            r = self%v
        end function
    end template
end module

module template_struct_extends_01_m2
    use template_struct_extends_01_m, only: tm, tr
    implicit none
    instantiate tm {integer}
    instantiate tr {integer, myop}
    ! Not the parent of `d_t`, which is not visible here.
    integer, private :: base_t = 1
contains
    integer function myop(x)
        integer, intent(in) :: x
        myop = x + 1000
    end function
end module

program template_struct_extends_01
    use template_struct_extends_01_m0, only: bump_t
    use template_struct_extends_01_m
    use template_struct_extends_01_m2
    implicit none
    instantiate tm {real}, only: c_real => c_t
    instantiate ta {integer}
    type(c_real) :: c
    type(a_t) :: a
    type(d_t) :: d
    type(e_t) :: e
    type(bump_t) :: bb
    type(y_t) :: y
    class(x_t), allocatable :: x

    if (f_int(1) /= 7) error stop
    if (g_int(5) /= 5) error stop

    c%n = 3
    c%v = 2.5
    c%k = 4
    if (c%n /= 3) error stop
    if (abs(c%v - 2.5) > 1e-6) error stop
    if (abs(c%get() - 2.5) > 1e-6) error stop
    if (c%k /= 4) error stop

    a%w = 6
    if (a%w /= 6) error stop
    if (a%z_t%w /= 6) error stop
    if (f(1) /= 7) error stop

    ! `base_t` is not visible in the module instantiating `d_t`.
    d%m = 8
    d%u = 9
    if (d%m /= 8) error stop
    if (d%u /= 9) error stop

    ! The parent's bound procedure is the module's, not an instance of it.
    e%n = 1
    e%v = 2
    call e%bump()
    call bb%bump()
    if (e%n /= 11) error stop
    if (bb%n /= 10) error stop

    ! The abstract parent's binding stays deferred.
    y%v = 4
    if (y%get() /= 4) error stop
    allocate(x, source=y)
    if (x%get() /= 4) error stop
    print *, "ok"
end program
