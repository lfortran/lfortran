module modules_15b
use iso_c_binding, only: c_int, c_long_long, c_float, c_double, c_char, &
    c_null_char, c_float_complex, c_double_complex, c_int32_t, c_int64_t
implicit none

interface
    ! int f_int_float(int *a, float *b)
    integer(c_int) function f_int_float(a, b) result(r) bind(c)
    import :: c_int, c_float
    integer(c_int), intent(in) :: a
    real(c_float), intent(in) :: b
    end function

    ! int f_int_double(int *a, double *b)
    integer(c_int) function f_int_double(a, b) result(r) bind(c)
    import :: c_int, c_double
    integer(c_int), intent(in) :: a
    real(c_double), intent(in) :: b
    end function

    ! int f_int_float_complex(int *a, float_complex_t *b)
    integer(c_int) function f_int_float_complex(a, b) result(r) bind(c)
    import :: c_int, c_float
    integer(c_int), intent(in) :: a
    complex(c_float), intent(in) :: b
    end function

    ! int f_int_double_complex(int *a, double_complex_t *b)
    integer(c_int) function f_int_double_complex(a, b) result(r) bind(c)
    import :: c_int, c_double
    integer(c_int), intent(in) :: a
    complex(c_double), intent(in) :: b
    end function

    integer(c_int) function f_int_float_complex2(a, b) result(r) &
            bind(c, name="f_int_float_complex")
    import :: c_int, c_float_complex
    integer(c_int), intent(in) :: a
    complex(c_float_complex), intent(in) :: b
    end function

    ! int f_int_double_complex(int *a, double_complex_t *b)
    integer(c_int) function f_int_double_complex2(a, b) result(r) &
            bind(c, name="f_int_double_complex")
    import :: c_int, c_double_complex
    integer(c_int), intent(in) :: a
    complex(c_double_complex), intent(in) :: b
    end function

    ! int f_int_float_complex_value(int a, float_complex_t b)
    integer(c_int) function f_int_float_complex_value(a, b) result(r) bind(c)
    import :: c_int, c_float
    integer(c_int), value, intent(in) :: a
    complex(c_float), value, intent(in) :: b
    end function

    ! int f_int_double_complex_value(int a, double_complex_t b)
    integer(c_int) function f_int_double_complex_value(a, b) result(r) bind(c)
    import :: c_int, c_double
    integer(c_int), value, intent(in) :: a
    complex(c_double), value, intent(in) :: b
    end function

    ! float_complex_t f_float_complex_value_return(float_complex_t b)
    complex(c_float) function f_float_complex_value_return(b) result(r) bind(c)
    import :: c_float
    complex(c_float), value, intent(in) :: b
    end function

    ! double_complex_t f_double_complex_value_return(double_complex_t b)
    complex(c_double) function f_double_complex_value_return(b) result(r)bind(c)
    import :: c_double
    complex(c_double), value, intent(in) :: b
    end function

    ! int f_int_float_value(int a, float b)
    integer(c_int) function f_int_float_value(a, b) result(r) bind(c)
    import :: c_int, c_float
    integer(c_int), value, intent(in) :: a
    real(c_float), value, intent(in) :: b
    end function

    ! int f_int_double_value(int a, double b)
    integer(c_int) function f_int_double_value(a, b) result(r) bind(c)
    import :: c_int, c_double
    integer(c_int), value, intent(in) :: a
    real(c_double), value, intent(in) :: b
    end function

    ! int f_int_intarray(int n, int *b)
    integer(c_int) function f_int_intarray(n, b) result(r) bind(c)
    import :: c_int
    integer(c_int), value, intent(in) :: n
    integer(c_int), intent(in) :: b(n)
    end function

    ! float f_int_floatarray(int n, float *b)
    real(c_float) function f_int_floatarray(n, b) result(r) bind(c)
    import :: c_int, c_float
    integer(c_int), value, intent(in) :: n
    real(c_float), intent(in) :: b(n)
    end function

    ! double f_int_doublearray(int n, double *b)
    real(c_double) function f_int_doublearray(n, b) result(r) bind(c)
    import :: c_int, c_double
    integer(c_int), value, intent(in) :: n
    real(c_double), intent(in) :: b(n)
    end function

    ! int f_int_intarray(int n, int *b)
    integer(c_int) function f_int_intarray_star(n, b) result(r) &
            bind(c, name="f_int_intarray")
    import :: c_int
    integer(c_int), value, intent(in) :: n
    integer(c_int), intent(in) :: b(*)
    end function

    ! float f_int_floatarray_star(int n, float *b)
    real(c_float) function f_int_floatarray_star(n, b) result(r) bind(c)
    import :: c_int, c_float
    integer(c_int), value, intent(in) :: n
    real(c_float), intent(in) :: b(*)
    end function

    ! double f_int_doublearray(int n, double *b)
    real(c_double) function f_int_doublearray_star(n, b) result(r) &
            bind(c, name="f_int_doublearray")
    import :: c_int, c_double
    integer(c_int), value, intent(in) :: n
    real(c_double), intent(in) :: b(*)
    end function

    ! int f_int_double_value(int a, double b)
    integer(c_int) function f_int_double_value_name(a, b) result(r) &
            bind(c, name="f_int_double_value")
    import :: c_int, c_double
    integer(c_int), value, intent(in) :: a
    real(c_double), value, intent(in) :: b
    end function

!----------------------------------------------------------------------------

    ! void sub_int_float(int *a, float *b, int *r)
    subroutine sub_int_float(a, b, r) bind(c)
    import :: c_int, c_float
    integer(c_int), intent(in) :: a
    real(c_float), intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine

    ! void sub_int_double(int *a, double *b, int *r)
    subroutine sub_int_double(a, b, r) bind(c)
    import :: c_int, c_double
    integer(c_int), intent(in) :: a
    real(c_double), intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine

    ! void sub_int_float_complex(int *a, float_complex_t *b, int *r)
    subroutine sub_int_float_complex(a, b, r) bind(c)
    import :: c_int, c_float
    integer(c_int), intent(in) :: a
    complex(c_float), intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine

    ! void sub_int_double_complex(int *a, double_complex_t *b, int *r)
    subroutine sub_int_double_complex(a, b, r) bind(c)
    import :: c_int, c_double
    integer(c_int), intent(in) :: a
    complex(c_double), intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine

    ! void sub_int_float_value(int a, float b, int *r)
    subroutine sub_int_float_value(a, b, r) bind(c)
    import :: c_int, c_float
    integer(c_int), value, intent(in) :: a
    real(c_float), value, intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine

    ! void sub_int_double_value(int a, double b, int *r)
    subroutine sub_int_double_value(a, b, r) bind(c)
    import :: c_int, c_double
    integer(c_int), value, intent(in) :: a
    real(c_double), value, intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine

    ! void sub_int_float_complex_value(int a, float_complex_t b, int *r)
    subroutine sub_int_float_complex_value(a, b, r) bind(c)
    import :: c_int, c_float
    integer(c_int), value, intent(in) :: a
    complex(c_float), value, intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine

    ! void sub_int_double_complex_value(int a, double_complex_t b, int *r)
    subroutine sub_int_double_complex_value(a, b, r) bind(c)
    import :: c_int, c_double
    integer(c_int), value, intent(in) :: a
    complex(c_double), value, intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine

    ! void sub_int_intarray(int n, int *b, int *r)
    subroutine sub_int_intarray(n, b, r) bind(c)
    import :: c_int
    integer(c_int), value, intent(in) :: n
    integer(c_int), intent(in) :: b(n)
    integer(c_int), intent(out) :: r
    end subroutine

    ! void sub_int_floatarray(int n, float *b, float *r)
    subroutine sub_int_floatarray(n, b, r) bind(c)
    import :: c_int, c_float
    integer(c_int), value, intent(in) :: n
    real(c_float), intent(in) :: b(n)
    real(c_float), intent(out) :: r
    end subroutine

    ! void sub_int_doublearray(int n, double *b, double *r)
    subroutine sub_int_doublearray(n, b, r) bind(c)
    import :: c_int, c_double
    integer(c_int), value, intent(in) :: n
    real(c_double), intent(in) :: b(n)
    real(c_double), intent(out) :: r
    end subroutine

    ! void sub_int_double_value(int a, double b, int *r)
    subroutine sub_int_double_value_name(a, b, r) &
        bind(c, name="sub_int_double_value")
    import :: c_int, c_double
    integer(c_int), value, intent(in) :: a
    real(c_double), value, intent(in) :: b
    integer(c_int), intent(out) :: r
    end subroutine

    ! int f_string(char *s)
    integer(c_int) function f_string0(s) result(r) bind(c, name="f_string")
    import :: c_int, c_char
    character(len=1, kind=c_char), intent(in) :: s
    end function

    integer(c_int) function call_fortran_i32(i) result(r) bind(c)
    import :: c_int
    integer(c_int), value, intent(in) :: i
    end function

    integer(c_int) function call_fortran_i32_value(i) result(r) bind(c)
    import :: c_int
    integer(c_int), value, intent(in) :: i
    end function

    integer(c_int32_t) function call_fortran_i32_value2(i) result(r) &
            bind(c, name="call_fortran_i32_value")
    import :: c_int32_t
    integer(c_int32_t), value, intent(in) :: i
    end function

    integer(c_long_long) function call_fortran_i64(i) result(r) bind(c)
    import :: c_long_long
    integer(c_long_long), value, intent(in) :: i
    end function

    integer(c_long_long) function call_fortran_i64_value(i) result(r) bind(c)
    import :: c_long_long
    integer(c_long_long), value, intent(in) :: i
    end function

    integer(c_int64_t) function call_fortran_i64_value2(i) result(r) &
            bind(c, name="call_fortran_i64_value")
    import :: c_int64_t
    integer(c_int64_t), value, intent(in) :: i
    end function

    real(c_float) function call_fortran_f32(i) result(r) bind(c)
    import :: c_float
    real(c_float), value, intent(in) :: i
    end function

    real(c_float) function call_fortran_f32_value(i) result(r) bind(c)
    import :: c_float
    real(c_float), value, intent(in) :: i
    end function

    real(c_double) function call_fortran_f64(i) result(r) bind(c)
    import :: c_double
    real(c_double), value, intent(in) :: i
    end function

    real(c_double) function call_fortran_f64_value(i) result(r) bind(c)
    import :: c_double
    real(c_double), value, intent(in) :: i
    end function

end interface

contains

integer function f_string(s) result(r)
character(*), intent(in) :: s
r = f_string0(s // c_null_char)
end function

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

end module
