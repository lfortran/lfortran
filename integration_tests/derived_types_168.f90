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

end module

program derived_types_168
use derived_types_168_m
implicit none

associate (q => pb)
    if (q%a%x /= 10) error stop
    if (q%y /= 30) error stop
end associate

associate (r => pb%a)
    if (r%x /= 10) error stop
end associate

end program
