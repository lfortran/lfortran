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
end associate

associate (q => vb)
    q%a%x = 21
    q%y = 41
end associate
if (vb%a%x /= 21) error stop
if (vb%y /= 41) error stop

associate (r => vb%a)
    r%x = 22
end associate
if (vb%a%x /= 22) error stop

end program
