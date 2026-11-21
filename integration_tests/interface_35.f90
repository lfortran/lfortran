use, intrinsic :: iso_c_binding, only: c_int
implicit none

integer(c_int) :: a, b

interface
    subroutine add_one(x)
        import :: c_int
        integer(c_int), intent(inout) :: x
    end subroutine
end interface

interface
    function double_val(x) result(res)
        import :: c_int
        integer(c_int), intent(in) :: x
        integer(c_int) :: res
    end function
end interface

integer(c_int) :: c

a = 5_c_int
call add_one(a)
if (a /= 6_c_int) error stop

b = double_val(a)
if (b /= 12_c_int) error stop

c = a + b
if (c /= 18_c_int) error stop

print *, a, b, c
end

subroutine add_one(x)
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(inout) :: x
    x = x + 1_c_int
end subroutine

function double_val(x) result(res)
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), intent(in) :: x
    integer(c_int) :: res
    res = 2_c_int * x
end function
