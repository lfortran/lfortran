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
        f = x + sum(args)
    end function g0

    function interval_max(fun, args, grid_size) result(x)
        implicit none
        !> 'fun' is a procedure variable
        procedure(FUNC_WITH_ARGS) :: fun
        real, intent(in) :: args(:)
        integer, intent(in) :: grid_size

        real :: x

        real :: k
        integer :: i
        real :: fgrid(grid_size)

        !> function call with procedure variable
        fgrid = [(fun(k, args), i=1, grid_size)]
        ! we return a random value
        x = sum(fgrid)

    end function interval_max
end module interface_19_mod

program interface_19
    use interface_19_mod
    implicit none
    real, allocatable :: args_i(:)
    integer, parameter :: grid_size_i = 5
    real :: x

    allocate(args_i(8))
    args_i = [1., 5., 3., 7., 8., 9., 10., 1.]

    x = interval_max(g0, args_i, grid_size_i)
    print *, x
    if (x /= 220.0_4) error stop
end program
