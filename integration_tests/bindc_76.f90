! The NAME= of BIND(C) is a constant expression (F2023 R808, C809), not only
! a literal: concatenations, named constants (also from a used module), and
! intrinsic functions and substrings of constants. Each binding label is the
! name of a C function or object in bindc_76.c, so a wrong label fails to link
! or gives a wrong value.
module bindc_76_names
    implicit none
    character(*), parameter :: used_pfx = "bindc_76_", used_suf = "used"
end module

module bindc_76_mod
    use iso_c_binding, only: c_int
    use bindc_76_names, only: used_pfx, used_suf
    implicit none
    character(*), parameter :: pfx = "bindc_76_"
    character(*), parameter :: substr_name = "bindc_76_substrxyz"

    abstract interface
        function int_fn(x) bind(C) result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
    end interface

    interface
        function f_concat(x) bind(C, name="bindc_76_"//"concat") result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
        function f_trim(x) bind(C, name=trim("bindc_76_trim   ")) result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
    end interface

    procedure(int_fn), bind(C, name=pfx//"proc") :: f_proc
    procedure(int_fn), bind(C, name=used_pfx//used_suf) :: f_used
    procedure(int_fn), bind(C, name=substr_name(1:15)) :: f_substr

    integer(c_int), bind(C, name=pfx//"var") :: v = 7
    integer(c_int) :: w = 9
    bind(C, name=pfx//"w") :: w
    integer(c_int) :: cv
    common /blk/ cv
    bind(C, name=pfx//"blk") :: /blk/

contains

    function twice(x) bind(C, name=pfx//"twice") result(r)
        integer(c_int), value :: x
        integer(c_int) :: r
        r = 2*x
    end function

    subroutine setcv(x) bind(C, name=pfx//"setcv")
        integer(c_int), value :: x
        cv = x
    end subroutine

end module

program bindc_76
    use iso_c_binding, only: c_int
    use bindc_76_mod
    implicit none
    interface
        function get_var() bind(C, name="bindc_76_get_var") result(r)
            import :: c_int
            integer(c_int) :: r
        end function
        function get_w() bind(C, name="bindc_76_get_w") result(r)
            import :: c_int
            integer(c_int) :: r
        end function
        function get_cv() bind(C, name="bindc_76_get_cv") result(r)
            import :: c_int
            integer(c_int) :: r
        end function
        function call_twice(x) bind(C, name="bindc_76_call_twice") result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
        subroutine call_setcv(x) bind(C, name="bindc_76_call_setcv")
            import :: c_int
            integer(c_int), value :: x
        end subroutine
    end interface

    if (f_concat(10) /= 11) error stop 1
    if (f_proc(10) /= 12) error stop 2
    if (f_used(10) /= 13) error stop 3
    if (f_trim(10) /= 14) error stop 4
    if (f_substr(10) /= 15) error stop 5
    if (get_var() /= 7) error stop 6
    if (get_w() /= 9) error stop 7
    call call_setcv(5)
    if (cv /= 5) error stop 8
    if (get_cv() /= 5) error stop 9
    if (call_twice(4) /= 8) error stop 10
    print *, "ok"
end program
