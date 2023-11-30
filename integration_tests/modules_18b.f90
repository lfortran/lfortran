module modules_18b
use iso_c_binding, only: c_int, c_long_long, c_float, c_double, c_char
implicit none

contains

integer function f(a, b) result(r)
integer, intent(in) :: a
real, intent(in) :: b
interface
    ! int f_int_float_value(int a, float b)
    integer(c_int) function f_int_float_value(a, b) result(r) bind(c)
    import :: c_int, c_float
    integer(c_int), value, intent(in) :: a
    real(c_float), value, intent(in) :: b
    end function
end interface
r = f_int_float_value(a, b)
end function

subroutine g(a, b, r)
integer, intent(in) :: a
real, intent(in) :: b
integer, intent(out) :: r
interface
    ! void sub_int_float_value(int a, float b, int *r)
    subroutine sub_int_float_value(a, b, r) bind(c)
    import :: c_int, c_float
    integer(c_int), value, intent(in) :: a
    real(c_float), value, intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine
end interface
call sub_int_float_value(a, b, r)
end subroutine

integer(c_int) function fortran_i32(i) result(r) bind(c)
integer(c_int), intent(in) :: i
r = i + 2
end function

integer(c_int) function fortran_i32_value(i) result(r) bind(c)
integer(c_int), value, intent(in) :: i
r = i + 2
end function

integer(c_long_long) function fortran_i64(i) result(r) bind(c)
integer(c_long_long), intent(in) :: i
r = i + 2
end function

integer(c_long_long) function fortran_i64_value(i) result(r) bind(c)
integer(c_long_long), value, intent(in) :: i
r = i + 2
end function

real(c_float) function fortran_f32(i) result(r) bind(c)
real(c_float), intent(in) :: i
r = i + 2.3_c_float
end function

real(c_float) function fortran_f32_value(i) result(r) bind(c)
real(c_float), value, intent(in) :: i
r = i + 2.3_c_float
end function

real(c_double) function fortran_f64(i) result(r) bind(c)
real(c_double), intent(in) :: i
r = i + 2.3_c_double
end function

real(c_double) function fortran_f64_value(i) result(r) bind(c)
real(c_double), value, intent(in) :: i
r = i + 2.3_c_double
end function

function c2s(n, x) result(y)
integer, intent(in) :: n
character, intent(in) :: x(*)
character(:), allocatable :: y
integer :: i
allocate(character(n) :: y)
do i = 1, n
    y(i:i) = x(i)
end do
end function

integer function f_len_string(s) result(r)
character(*), intent(in) :: s
r = len(s)
end function

integer(c_int) function fortran_string(n, s) result(r) bind(c)
integer(c_int), value, intent(in) :: n
character(len=1, kind=c_char), intent(in) :: s(*)
r = f_len_string(c2s(n, s))
end function

end module
