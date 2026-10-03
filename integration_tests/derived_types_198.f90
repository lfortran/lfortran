module derived_types_198_mod
    implicit none

    type :: inner_t
        integer :: v = 0
    end type inner_t

    type :: outer_t
        type(inner_t) :: a(2)
        integer :: h = 0
    end type outer_t

    type :: leaf_t
        integer :: w(2) = 0
        integer :: s = 0
    end type leaf_t

    type :: nest_t
        type(leaf_t) :: l(2)
        integer :: t = 0
    end type nest_t

    type :: deep_t
        type(outer_t) :: b(2)
        integer :: x = 0
    end type deep_t

    ! array component given element by element
    type(outer_t), parameter :: po = outer_t(a=[inner_t(5), inner_t(6)], h=8)
    ! array component given as a scalar, broadcast over the component
    type(outer_t), parameter :: ps = outer_t(a=inner_t(4), h=7)
    ! the type of the component array has an array component of its own
    type(nest_t), parameter :: pn = nest_t( &
        l=[leaf_t(w=[1, 2], s=11), leaf_t(w=[3, 4], s=12)], t=9)
    ! three levels of member selection, two of them array elements
    type(deep_t), parameter :: pd = deep_t(b=[ &
        outer_t(a=[inner_t(1), inner_t(2)], h=10), &
        outer_t(a=[inner_t(3), inner_t(4)], h=20)], x=100)
    ! the same declaration as a saved variable, which already worked
    type(outer_t), save :: so = outer_t(a=[inner_t(5), inner_t(6)], h=8)

    ! a member of a named constant used to initialize another named constant
    integer, parameter :: k1 = po%a(2)%v
    integer, parameter :: k2 = pn%l(1)%s
    integer, parameter :: k3 = pd%b(2)%a(2)%v

end module derived_types_198_mod

program derived_types_198
    use derived_types_198_mod
    implicit none
    integer :: e

    if (po%a(1)%v /= 5) error stop
    if (po%a(2)%v /= 6) error stop
    if (po%h /= 8) error stop

    if (ps%a(1)%v /= 4) error stop
    if (ps%a(2)%v /= 4) error stop
    if (ps%h /= 7) error stop

    if (pn%l(1)%w(2) /= 2) error stop
    if (pn%l(2)%w(1) /= 3) error stop
    if (pn%l(1)%s /= 11) error stop
    if (pn%l(2)%s /= 12) error stop
    if (pn%t /= 9) error stop

    if (pd%b(1)%a(2)%v /= 2) error stop
    if (pd%b(2)%a(1)%v /= 3) error stop
    if (pd%b(2)%h /= 20) error stop
    if (pd%x /= 100) error stop

    if (k1 /= 6) error stop
    if (k2 /= 11) error stop
    if (k3 /= 4) error stop

    ! in an expression
    e = po%a(1)%v * 10 + po%a(2)%v + po%h
    if (e /= 64) error stop

    ! the saved variable spelling still works
    if (so%a(1)%v /= 5) error stop
    if (so%a(2)%v /= 6) error stop
    if (so%h /= 8) error stop

    print *, "ok"
end program derived_types_198
