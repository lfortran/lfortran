! Leading and trailing blanks of the NAME= of BIND(C) are not part of the
! binding label (F2023 18.10.2), whether NAME= is a literal or a constant
! expression. Each binding label is the name of a C function or object in
! bindc_77.c, so a label that keeps its blanks fails to link. A procedure
! or common block whose NAME= is all blanks has no binding label (18.10.2.2).
module bindc_77_mod
    use iso_c_binding, only: c_int
    implicit none
    character(*), parameter :: pfx = "bindc_77_"

    abstract interface
        function int_fn(x) bind(C) result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
    end interface

    interface
        function f_lit(x) bind(C, name="  bindc_77_lit  ") result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
        function f_expr(x) bind(C, name="  bindc_77_"//"expr  ") result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
    end interface

    procedure(int_fn), bind(C, name=" "//pfx//"proc ") :: f_proc

    integer(c_int), bind(C, name=" bindc_77_var ") :: v = 7
    integer(c_int) :: w = 9
    bind(C, name="  "//pfx//"w  ") :: w
    integer(c_int) :: cv
    common /blk/ cv
    bind(C, name=" "//pfx//"blk") :: /blk/
    integer(c_int) :: cv2
    common /blk2/ cv2
    bind(C, name="  ") :: /blk2/

contains

    function twice(x) bind(C, name=pfx//"twice   ") result(r)
        integer(c_int), value :: x
        integer(c_int) :: r
        r = 2*x
    end function

    function no_label(x) bind(C, name="   ") result(r)
        integer(c_int), value :: x
        integer(c_int) :: r
        r = x + 100
    end function

    subroutine bump_cv2()
        cv2 = cv2 + 1
    end subroutine

end module

program bindc_77
    use iso_c_binding, only: c_int
    use bindc_77_mod
    implicit none
    interface
        function get_var() bind(C, name="bindc_77_get_var") result(r)
            import :: c_int
            integer(c_int) :: r
        end function
        function get_w() bind(C, name="bindc_77_get_w") result(r)
            import :: c_int
            integer(c_int) :: r
        end function
        function get_cv() bind(C, name="bindc_77_get_cv") result(r)
            import :: c_int
            integer(c_int) :: r
        end function
        function call_twice(x) bind(C, name="bindc_77_call_twice") result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
    end interface

    if (f_lit(10) /= 11) error stop 1
    if (f_expr(10) /= 12) error stop 2
    if (f_proc(10) /= 13) error stop 3
    if (get_var() /= 7) error stop 4
    if (get_w() /= 9) error stop 5
    cv = 5
    if (get_cv() /= 5) error stop 6
    if (call_twice(4) /= 8) error stop 7
    if (no_label(1) /= 101) error stop 8
    cv2 = 20
    call bump_cv2()
    if (cv2 /= 21) error stop 9
    print *, "ok"
end program
