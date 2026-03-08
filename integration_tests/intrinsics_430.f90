program intrinsics_430
implicit none

type :: t1
    real :: x
end type

type(t1), target :: a(5)
a%x = 1.0

if (.not. is_contiguous(a)) error stop
if (is_contiguous(a%x)) error stop

associate(x => a%x)
    if (.not. is_contiguous(x)) error stop
end associate

block
    real, pointer :: x(:)
    x => a%x
    if (.not. is_contiguous(x)) error stop
end block

if (.not. is_dummy_arg_contiguous(a%x)) error stop

contains
    function is_dummy_arg_contiguous(a) result(r)
        real, intent(in) :: a(:)
        logical :: r
        r = is_contiguous(a)
    end function
end program intrinsics_430