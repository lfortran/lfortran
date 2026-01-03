module interface_19_mod
    implicit none

    abstract interface
        function FUNC_WITH_ARGS(x, args) result(f)
        implicit none
        real, intent(in) :: x
        real, intent(in) :: args(:)
        real :: f
        end function FUNC_WITH_ARGS
    end interface

contains

    ! an implementation of the abstract interface 'FUNC_WITH_ARGS'
    function g0(x, args) result(f)
        implicit none
        real, intent(in) :: x
        real, intent(in) :: args(:)
        real :: f

        f = args(1)*x**2 + args(2)*x + args(3)
    end function g0

    function interval_max(fun, args, grid_size) result(x)
        implicit none
        procedure(FUNC_WITH_ARGS) :: fun
        real, intent(in) :: args(:)
        integer, intent(in) :: grid_size

        real :: x
        real :: k
        integer :: i
        real :: fgrid(grid_size)

        do i = 1, grid_size
            k = real(i) / grid_size
            !> function call with procedure variable
            fgrid(i) = fun(k, args)
        end do

        x = maxval(fgrid)
    end function interval_max

end module interface_19_mod

program interface_19
    use interface_19_mod
    implicit none

    real :: res
    real :: args_i(3) = [1.0, -2.0, 1.0]
    integer, parameter :: grid_size_i = 10

    res = interval_max(g0, args_i, grid_size_i)
    print *, res
    if (abs(res - 0.81_4) > 1.0e-13) error stop
end program
