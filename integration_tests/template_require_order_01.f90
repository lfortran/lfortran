! Tests that a REQUIREMENT's deferred-argument list may be given in any order,
! in particular that a deferred procedure may appear before the deferred types
! its interface refers to (lfortran/lfortran#11378).
!
! Each requirement below has the same body -- t, u and v are declared first and
! op_func refers to t and v -- and only the order of the deferred-argument list
! differs.  Every ordering must compile and behave identically.
module template_require_order_01_m
    implicit none
    private
    public :: test_require_order

    ! procedure before every type
    requirement op_r_a {op_func, t, u, v}
        deferred type :: t
        deferred type :: u
        deferred type :: v
        deferred interface
            pure function op_func(lhs, rhs) result(res)
                type(t), intent(in) :: lhs
                type(t), intent(in) :: rhs
                type(v) :: res
            end function
        end interface
    end requirement

    ! two procedures before every type
    requirement op_r_b {op_func, op_func2, t, u, v}
        deferred type :: t
        deferred type :: u
        deferred type :: v
        deferred interface
            pure function op_func(lhs, rhs) result(res)
                type(t), intent(in) :: lhs
                type(t), intent(in) :: rhs
                type(v) :: res
            end function
            pure function op_func2(lhs, rhs) result(res)
                type(u), intent(in) :: lhs
                type(u), intent(in) :: rhs
                type(u) :: res
            end function
        end interface
    end requirement

    ! procedure in the middle, with v (which it uses) still to come
    requirement op_r_c {t, u, op_func, v}
        deferred type :: t
        deferred type :: u
        deferred type :: v
        deferred interface
            pure function op_func(lhs, rhs) result(res)
                type(t), intent(in) :: lhs
                type(t), intent(in) :: rhs
                type(v) :: res
            end function
        end interface
    end requirement

    ! procedure in the middle, with both types it uses already given
    requirement op_r_d {t, v, op_func, u}
        deferred type :: t
        deferred type :: u
        deferred type :: v
        deferred interface
            pure function op_func(lhs, rhs) result(res)
                type(t), intent(in) :: lhs
                type(t), intent(in) :: rhs
                type(v) :: res
            end function
        end interface
    end requirement

    ! procedure last
    requirement op_r_e {t, u, v, op_func}
        deferred type :: t
        deferred type :: u
        deferred type :: v
        deferred interface
            pure function op_func(lhs, rhs) result(res)
                type(t), intent(in) :: lhs
                type(t), intent(in) :: rhs
                type(v) :: res
            end function
        end interface
    end requirement

    template tmpl_a(t, u, v, f)
        deferred type :: t, u, v
        require :: op_r_a {f, t, u, v}
        private
        public :: apply
      contains
        function apply(a, b) result(res)
            type(t), intent(in) :: a
            type(t), intent(in) :: b
            type(v) :: res
            res = f(a, b)
        end function
    end template

    template tmpl_b(t, u, v, f, g)
        deferred type :: t, u, v
        require :: op_r_b {f, g, t, u, v}
        private
        public :: apply, apply2
      contains
        function apply(a, b) result(res)
            type(t), intent(in) :: a
            type(t), intent(in) :: b
            type(v) :: res
            res = f(a, b)
        end function
        function apply2(a, b) result(res)
            type(u), intent(in) :: a
            type(u), intent(in) :: b
            type(u) :: res
            res = g(a, b)
        end function
    end template

    template tmpl_c(t, u, v, f)
        deferred type :: t, u, v
        require :: op_r_c {t, u, f, v}
        private
        public :: apply
      contains
        function apply(a, b) result(res)
            type(t), intent(in) :: a
            type(t), intent(in) :: b
            type(v) :: res
            res = f(a, b)
        end function
    end template

    template tmpl_d(t, u, v, f)
        deferred type :: t, u, v
        require :: op_r_d {t, v, f, u}
        private
        public :: apply
      contains
        function apply(a, b) result(res)
            type(t), intent(in) :: a
            type(t), intent(in) :: b
            type(v) :: res
            res = f(a, b)
        end function
    end template

    template tmpl_e(t, u, v, f)
        deferred type :: t, u, v
        require :: op_r_e {t, u, v, f}
        private
        public :: apply
      contains
        function apply(a, b) result(res)
            type(t), intent(in) :: a
            type(t), intent(in) :: b
            type(v) :: res
            res = f(a, b)
        end function
    end template

contains

    pure function add_ii(lhs, rhs) result(res)
        integer, intent(in) :: lhs
        integer, intent(in) :: rhs
        integer :: res
        res = lhs + rhs
    end function

    pure function mul_rr(lhs, rhs) result(res)
        real, intent(in) :: lhs
        real, intent(in) :: rhs
        real :: res
        res = lhs * rhs
    end function

    subroutine test_require_order()
        instantiate tmpl_a {integer, real, integer, add_ii}, only: apply_a => apply
        instantiate tmpl_b {integer, real, integer, add_ii, mul_rr}, &
            only: apply_b => apply, apply2_b => apply2
        instantiate tmpl_c {integer, real, integer, add_ii}, only: apply_c => apply
        instantiate tmpl_d {integer, real, integer, add_ii}, only: apply_d => apply
        instantiate tmpl_e {integer, real, integer, add_ii}, only: apply_e => apply
        integer :: i
        real :: r

        i = apply_a(3, 4)
        if (i /= 7) error stop

        i = apply_b(5, 6)
        if (i /= 11) error stop
        r = apply2_b(2.0, 3.0)
        if (abs(r - 6.0) > 1e-6) error stop

        i = apply_c(7, 8)
        if (i /= 15) error stop

        i = apply_d(9, 10)
        if (i /= 19) error stop

        i = apply_e(11, 12)
        if (i /= 23) error stop
    end subroutine

end module

program template_require_order_01
    use template_require_order_01_m
    implicit none

    call test_require_order()

end program
