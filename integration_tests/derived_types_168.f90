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
type(b_t) :: module_array(2)

contains

subroutine check_dummy_array(dummy_array)
    type(b_t), intent(inout) :: dummy_array(:)

    associate (r => dummy_array(1))
        r%a%x = 101
        r%y = 102
    end associate
    associate (s => dummy_array(1:2))
        s(2)%a%x = 103
        s(2)%y = 104
    end associate
    if (dummy_array(1)%a%x /= 101) error stop
    if (dummy_array(1)%y /= 102) error stop
    if (dummy_array(2)%a%x /= 103) error stop
    if (dummy_array(2)%y /= 104) error stop
end subroutine

subroutine check_module_array()
    module_array(1)%a%x = 1
    module_array(1)%y = 2
    module_array(2)%a%x = 3
    module_array(2)%y = 4

    associate (r => module_array(1))
        r%a%x = 201
        r%y = 202
    end associate
    associate (s => module_array(1:2))
        s(2)%a%x = 203
        s(2)%y = 204
    end associate
    if (module_array(1)%a%x /= 201) error stop
    if (module_array(1)%y /= 202) error stop
    if (module_array(2)%a%x /= 203) error stop
    if (module_array(2)%y /= 204) error stop
end subroutine

end module

program derived_types_168
use derived_types_168_m
implicit none
type(b_t) :: vb
type(b_t) :: ordinary_array(2)
type(b_t), allocatable :: alloc_array(:)
type(b_t), pointer :: pointer_array(:)
integer :: i

vb = b_t(a_t(20), 40)
i = 1

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

associate (x => pba(1)%a%x)
    if (x /= 11) error stop
end associate

associate (x => pba(i)%a%x)
    if (x /= 11) error stop
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

ordinary_array = [b_t(a_t(1), 2), b_t(a_t(3), 4)]
associate (r => ordinary_array(1))
    r%a%x = 301
    r%y = 302
end associate
associate (s => ordinary_array(1:2))
    s(2)%a%x = 303
    s(2)%y = 304
end associate
if (ordinary_array(1)%a%x /= 301) error stop
if (ordinary_array(1)%y /= 302) error stop
if (ordinary_array(2)%a%x /= 303) error stop
if (ordinary_array(2)%y /= 304) error stop

allocate(alloc_array(2))
alloc_array = [b_t(a_t(1), 2), b_t(a_t(3), 4)]
associate (r => alloc_array(1))
    r%a%x = 401
    r%y = 402
end associate
associate (s => alloc_array(1:2))
    s(2)%a%x = 403
    s(2)%y = 404
end associate
if (alloc_array(1)%a%x /= 401) error stop
if (alloc_array(1)%y /= 402) error stop
if (alloc_array(2)%a%x /= 403) error stop
if (alloc_array(2)%y /= 404) error stop

allocate(pointer_array(2))
pointer_array = [b_t(a_t(1), 2), b_t(a_t(3), 4)]
associate (r => pointer_array(1))
    r%a%x = 501
    r%y = 502
end associate
associate (s => pointer_array(1:2))
    s(2)%a%x = 503
    s(2)%y = 504
end associate
if (pointer_array(1)%a%x /= 501) error stop
if (pointer_array(1)%y /= 502) error stop
if (pointer_array(2)%a%x /= 503) error stop
if (pointer_array(2)%y /= 504) error stop

call check_dummy_array(ordinary_array)
call check_module_array()

deallocate(pointer_array)

end program
