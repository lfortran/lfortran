! Test: BIND(C) derived type returned by value and passed by VALUE
!
! Features tested:
!   - C function returning a BIND(C) derived type by value
!   - C function returning a nested BIND(C) derived type by value
!   - Passing BIND(C) derived type by VALUE to C function
!   - Struct return value used directly as function argument

program bindc_43
    use iso_c_binding
    implicit none

    type, bind(C) :: point_t
        real(c_float) :: x, y
    end type

    type, bind(C) :: rect_t
        type(point_t) :: origin
        real(c_float) :: width, height
    end type

    interface
        type(point_t) function c43_make_point(x, y) bind(C)
            import :: c_float, point_t
            real(c_float), value :: x, y
        end function

        type(rect_t) function c43_make_rect(ox, oy, w, h) bind(C)
            import :: c_float, rect_t
            real(c_float), value :: ox, oy, w, h
        end function

        real(c_float) function c43_point_dist_sq(a, b) bind(C)
            import :: c_float, point_t
            type(point_t), value :: a, b
        end function

        real(c_float) function c43_rect_area(r) bind(C)
            import :: c_float, rect_t
            type(rect_t), value :: r
        end function

        type(point_t) function c43_add_points(a, b) bind(C)
            import :: point_t
            type(point_t), value :: a, b
        end function
    end interface

    type(point_t) :: p1, p2, p3
    type(rect_t) :: r
    real(c_float) :: d, area

    ! Test 1: Simple struct return by value
    p1 = c43_make_point(3.0_c_float, 4.0_c_float)
    if (abs(p1%x - 3.0) > 1e-5) error stop "FAIL: make_point x"
    if (abs(p1%y - 4.0) > 1e-5) error stop "FAIL: make_point y"

    ! Test 2: Nested struct return by value
    r = c43_make_rect(1.0_c_float, 2.0_c_float, 10.0_c_float, 5.0_c_float)
    if (abs(r%origin%x - 1.0) > 1e-5) error stop "FAIL: rect origin x"
    if (abs(r%origin%y - 2.0) > 1e-5) error stop "FAIL: rect origin y"
    if (abs(r%width - 10.0) > 1e-5)   error stop "FAIL: rect width"
    if (abs(r%height - 5.0) > 1e-5)   error stop "FAIL: rect height"

    ! Test 3: Struct passed by VALUE (squared distance)
    p2 = c43_make_point(0.0_c_float, 0.0_c_float)
    d = c43_point_dist_sq(p1, p2)
    if (abs(d - 25.0) > 1e-5) error stop "FAIL: dist_sq"

    ! Test 4: Nested struct passed by VALUE (area)
    area = c43_rect_area(r)
    if (abs(area - 50.0) > 1e-5) error stop "FAIL: rect_area"

    ! Test 5: Return value used as function argument
    p3 = c43_add_points(p1, c43_make_point(7.0_c_float, 6.0_c_float))
    if (abs(p3%x - 10.0) > 1e-5) error stop "FAIL: add x"
    if (abs(p3%y - 10.0) > 1e-5) error stop "FAIL: add y"

    print *, "All bindc_43 tests passed."
end program
