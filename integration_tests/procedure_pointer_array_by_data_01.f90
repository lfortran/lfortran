module ppabd_types
    implicit none
    type, abstract :: rk_t
        integer :: n = 0
        procedure(event_func), pointer :: g => null()
    end type
    abstract interface
        subroutine event_func(me, t, x, g)
            import :: rk_t
            class(rk_t), intent(inout) :: me
            real(8), intent(in) :: t
            real(8), dimension(:), intent(in) :: x
            real(8), intent(out) :: g
        end subroutine
    end interface
    type, abstract, extends(rk_t) :: fixed_t
    contains
        procedure :: integrate => integrate_fixed
    end type
    type, extends(fixed_t) :: euler_t
    end type
contains
    subroutine integrate_fixed(me, t0, x0, gf)
        class(fixed_t), intent(inout) :: me
        real(8), intent(in) :: t0
        real(8), dimension(:), intent(in) :: x0
        real(8), intent(out) :: gf
        real(8), dimension(me%n) :: g_xf
        real(8) :: g
        call me%g(t0, x0, gf)
        g_xf = x0
        g = solver_func(0.5d0)
        gf = gf + g
    contains
        function solver_func(delt) result(r)
            real(8), intent(in) :: delt
            real(8) :: r
            call me%g(t0 + delt, g_xf, r)
        end function
    end subroutine
end module

program procedure_pointer_array_by_data_01
    use ppabd_types
    implicit none
    type(euler_t) :: a
    real(8) :: x(3), gf
    x = [1.0d0, 2.0d0, 3.0d0]
    a%n = 3
    a%g => my_g
    call a%integrate(0.5d0, x, gf)
    if (abs(gf - 13.5d0) > 1.0d-12) error stop 1
    print *, "ok"
contains
    subroutine my_g(me, t, x, g)
        class(rk_t), intent(inout) :: me
        real(8), intent(in) :: t
        real(8), dimension(:), intent(in) :: x
        real(8), intent(out) :: g
        g = sum(x) + t
    end subroutine
end program
