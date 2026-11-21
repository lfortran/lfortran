! An absent non-VALUE optional argument of a bind(c) procedure is passed
! as a null pointer of the type its dummy has in the procedure signature,
! both to C functions and to bind(c) module procedures.
module bindc_optional_absent_01_m
    use iso_c_binding, only: c_int, c_float, c_ptr
    implicit none
    type, bind(c) :: pt
        integer(c_int) :: a, b
    end type
    abstract interface
        subroutine cb_t(x) bind(c)
            import :: c_int
            integer(c_int), value :: x
        end subroutine
    end interface
    interface
        subroutine ext(x, y, s) bind(c, name="bindc_optional_absent_01_ext")
            import :: c_int
            integer(c_int), intent(in) :: x
            integer(c_int), intent(in), optional :: y
            integer(c_int), intent(out) :: s
        end subroutine
        integer(c_int) function chk_real(r) bind(c, name="bindc_optional_absent_01_real")
            import :: c_int, c_float
            real(c_float), intent(inout), optional :: r
        end function
        subroutine chk_pt(t, s) bind(c, name="bindc_optional_absent_01_pt")
            import :: c_int, pt
            type(pt), intent(in), optional :: t
            integer(c_int), intent(out) :: s
        end subroutine
        subroutine chk_cptr(p, s) bind(c, name="bindc_optional_absent_01_cptr")
            import :: c_int, c_ptr
            type(c_ptr), optional :: p
            integer(c_int), intent(out) :: s
        end subroutine
        subroutine chk_proc(cb, s) bind(c, name="bindc_optional_absent_01_proc")
            import :: c_int, cb_t
            procedure(cb_t), optional :: cb
            integer(c_int), intent(out) :: s
        end subroutine
        integer(c_int) function chk_proc_f(cb) bind(c, name="bindc_optional_absent_01_proc_f")
            import :: c_int, cb_t
            procedure(cb_t), optional :: cb
        end function
    end interface
    integer :: calls = 0
contains
    subroutine cb_add(x) bind(c)
        integer(c_int), value :: x
        calls = calls + x
    end subroutine

    subroutine sub_proc(cb) bind(c)
        procedure(cb_t), optional :: cb
        calls = calls + 1
    end subroutine

    integer(c_int) function fun_proc(cb) bind(c)
        procedure(cb_t), optional :: cb
        fun_proc = 7
    end function

    subroutine sub_cptr(p) bind(c)
        type(c_ptr), optional :: p
        calls = calls + 1
    end subroutine

    subroutine sub_cptr_in(p) bind(c)
        type(c_ptr), intent(in), optional :: p
        calls = calls + 1
    end subroutine

    subroutine sub_cptr_inout(p) bind(c)
        type(c_ptr), intent(inout), optional :: p
        calls = calls + 1
    end subroutine

    subroutine sub_int(x, i) bind(c)
        integer(c_int), intent(in) :: x
        integer(c_int), intent(in), optional :: i
        calls = calls + x
    end subroutine

    subroutine sub_real(r) bind(c)
        real(c_float), intent(inout), optional :: r
        calls = calls + 1
    end subroutine

    subroutine sub_pt(t) bind(c)
        type(pt), intent(in), optional :: t
        calls = calls + 1
    end subroutine
end module

program bindc_optional_absent_01
    use iso_c_binding, only: c_int, c_float, c_ptr, c_null_ptr
    use bindc_optional_absent_01_m
    implicit none
    integer(c_int) :: s
    real(c_float) :: r
    type(pt) :: t
    type(c_ptr) :: p

    call ext(2, 3, s)
    if (s /= 5) error stop 1
    call ext(2, s=s)
    if (s /= 2) error stop 2

    r = 1.5
    if (chk_real(r) /= 1) error stop 3
    if (abs(r - 3.0) > 1e-6) error stop 4
    if (chk_real() /= 0) error stop 5

    t = pt(4, 5)
    call chk_pt(t, s)
    if (s /= 9) error stop 6
    call chk_pt(s=s)
    if (s /= -1) error stop 7

    p = c_null_ptr
    call chk_cptr(p, s)
    if (s /= 1) error stop 8
    call chk_cptr(s=s)
    if (s /= 0) error stop 9

    call sub_cptr()
    call sub_cptr_in()
    call sub_cptr_inout()
    call sub_int(10)
    call sub_real()
    call sub_pt()
    if (calls /= 15) error stop 10

    call chk_proc(cb_add, s)
    if (s /= 1 .or. calls /= 20) error stop 11
    call chk_proc(s=s)
    if (s /= 0) error stop 12
    if (chk_proc_f(cb_add) /= 1 .or. calls /= 25) error stop 13
    if (chk_proc_f() /= 0) error stop 14
    call sub_proc()
    if (fun_proc() /= 7) error stop 15
    if (calls /= 26) error stop 16
    print *, "ok"
end program
