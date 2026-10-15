! A structure constructor may give a value to a private component only inside
! the module that defines the component and its descendants (F2018 7.5.4.8,
! 7.5.10 C7102). Each constructor here gives values only to accessible
! components: private components inside their module, its procedures, an
! internal procedure, a BLOCK construct and submodules, omitted private
! components that are default initialized or allocatable, public components
! of a type that also has private ones, the parent component of a public
! parent type, and components of a type in a module that is private by
! default. A public generic interface named like the type overloads its
! structure constructor with a private specific procedure.
module structure_constructor_args_30_m
    implicit none
    private
    public :: point_t, settings_t, mixed_t, base_t, open_t, counter_t, segment_t
    public :: origin, make_point, scaled_point, point_sum, settings_level
    public :: make_base, base_value, counter_value

    type :: point_t
        private
        integer :: x
        integer :: y = 2
    end type

    type :: settings_t
        private
        integer :: level = 1
        real, allocatable :: weights(:)
    end type

    type :: mixed_t
        integer :: p
        integer, private :: q = 3
    end type

    type :: base_t
        private
        integer :: v = 5
    end type

    type :: open_t
        integer :: a
    end type

    type :: counter_t
        private
        integer :: n
    end type

    interface counter_t
        procedure :: new_counter
    end interface

    type :: segment_t
        type(point_t) :: first = point_t(x=1, y=1)
    end type

    type(point_t), parameter :: origin = point_t(0, 0)

    interface
        module function scaled_point(p, k) result(r)
            type(point_t), intent(in) :: p
            integer, intent(in) :: k
            type(point_t) :: r
        end function
        module function shifted_point(p) result(r)
            type(point_t), intent(in) :: p
            type(point_t) :: r
        end function
    end interface

contains

    function make_point(x) result(r)
        integer, intent(in) :: x
        type(point_t) :: r
        r = point_t(x)
        block
            r = point_t(x=r%x, y=r%y + offset(1))
        end block
    contains
        integer function offset(k)
            integer, intent(in) :: k
            type(point_t) :: d
            d = point_t(k, k)
            offset = d%y
        end function
    end function

    integer function point_sum(p)
        type(point_t), intent(in) :: p
        point_sum = 100*p%x + p%y
    end function

    integer function settings_level(s)
        type(settings_t), intent(in) :: s
        settings_level = s%level
        if (allocated(s%weights)) settings_level = -1
    end function

    function make_base(v) result(r)
        integer, intent(in) :: v
        type(base_t) :: r
        r = base_t(v)
    end function

    integer function base_value(b)
        class(base_t), intent(in) :: b
        base_value = b%v
    end function

    function new_counter(n) result(r)
        integer, intent(in) :: n
        type(counter_t) :: r
        r%n = 10*n
    end function

    integer function counter_value(c)
        type(counter_t), intent(in) :: c
        counter_value = c%n
    end function

end module

submodule (structure_constructor_args_30_m) structure_constructor_args_30_s
    implicit none
contains
    module function scaled_point(p, k) result(r)
        type(point_t), intent(in) :: p
        integer, intent(in) :: k
        type(point_t) :: r
        r = shifted_point(point_t(p%x*k, y=p%y*k))
    end function
end submodule

submodule (structure_constructor_args_30_m:structure_constructor_args_30_s) &
        structure_constructor_args_30_t
    implicit none
contains
    module function shifted_point(p) result(r)
        type(point_t), intent(in) :: p
        type(point_t) :: r
        r = point_t(y=p%y, x=p%x + 1)
    end function
end submodule

module structure_constructor_args_30_ext
    use structure_constructor_args_30_m, only: base_t, make_base
    implicit none
    private
    public :: ext_t, make_ext

    type, extends(base_t) :: ext_t
        integer :: w
    end type

contains

    function make_ext(w) result(r)
        integer, intent(in) :: w
        type(ext_t) :: r
        r = ext_t(w=w)
        r = ext_t(base_t=make_base(r%w + 10), w=r%w)
    end function

end module

program structure_constructor_args_30
    use structure_constructor_args_30_m
    use structure_constructor_args_30_ext
    implicit none
    type(point_t) :: pt
    type(settings_t) :: s
    type(mixed_t) :: m
    type(open_t) :: o
    type(counter_t) :: c
    type(segment_t) :: seg
    type(ext_t) :: e

    pt = make_point(3)
    if (point_sum(pt) /= 303) error stop 1
    if (point_sum(origin) /= 0) error stop 2
    pt = scaled_point(pt, 2)
    if (point_sum(pt) /= 706) error stop 3
    if (point_sum(seg%first) /= 101) error stop 4

    s = settings_t()
    if (settings_level(s) /= 1) error stop 5

    m = mixed_t(7)
    if (m%p /= 7) error stop 6
    m = mixed_t(p=8)
    if (m%p /= 8) error stop 7

    o = open_t(4)
    if (o%a /= 4) error stop 8
    o = open_t(a=5)
    if (o%a /= 5) error stop 9

    c = counter_t(6)
    if (counter_value(c) /= 60) error stop 10
    c = counter_t(n=7)
    if (counter_value(c) /= 70) error stop 11

    e = ext_t(w=2)
    if (base_value(e) /= 5 .or. e%w /= 2) error stop 12
    e = ext_t(base_t=make_base(9), w=3)
    if (base_value(e) /= 9 .or. e%w /= 3) error stop 13
    e = make_ext(4)
    if (base_value(e) /= 14 .or. e%w /= 4) error stop 14

    print *, point_sum(pt), counter_value(c), base_value(e)
end program
