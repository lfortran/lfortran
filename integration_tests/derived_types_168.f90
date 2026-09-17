module derived_types_168_m
implicit none

type :: a_t
    integer :: x
end type

type :: b_t
    type(a_t) :: a
    integer :: y
end type

type(b_t), parameter :: pb = b_t(a_t(10), 30)
type(b_t), parameter :: pba(2) = [b_t(a_t(11), 31), b_t(a_t(12), 32)]

end module

program derived_types_168
use derived_types_168_m
implicit none
type(b_t) :: vb

vb = b_t(a_t(20), 40)

associate (q => pb)
    if (q%a%x /= 10) error stop
    if (q%y /= 30) error stop
    associate (r => q)
        if (r%a%x /= 10) error stop
        if (r%y /= 30) error stop
    end associate
    associate (r => q%a)
        if (r%x /= 10) error stop
    end associate
    associate (x => q%a%x)
        if (x /= 10) error stop
    end associate
end associate

associate (r => pb%a)
    if (r%x /= 10) error stop
end associate

associate (x => pb%a%x)
    if (x /= 10) error stop
end associate

associate (q => pba)
    if (q(1)%a%x /= 11) error stop
    if (q(1)%y /= 31) error stop
    if (q(2)%a%x /= 12) error stop
    if (q(2)%y /= 32) error stop
    associate (r => q(1))
        if (r%a%x /= 11) error stop
        if (r%y /= 31) error stop
    end associate
    associate (r => q(2)%a)
        if (r%x /= 12) error stop
    end associate
end associate

associate (q => vb)
    q%a%x = 21
    q%y = 41
    associate (r => q)
        r%a%x = 23
        r%y = 43
    end associate
end associate
if (vb%a%x /= 23) error stop
if (vb%y /= 43) error stop

associate (r => vb%a)
    r%x = 22
end associate
if (vb%a%x /= 22) error stop

associate (q => vb)
    associate (r => q%a)
        r%x = 24
    end associate
end associate
if (vb%a%x /= 24) error stop

end program
