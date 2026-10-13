! LFortran traits extension test; GFortran does not accept this syntax.
! A separately compiled provider (traits_runtime_numeric_03_provider.f90)
! supplies a closed numeric message, a message with two closed binders (one
! slot per member pair, the second selected by the sign of a scalar), and
! ordinary runtime messages with complex 2-D, real and lower-bound arrays
! and real and logical results. Generic
! procedures accept conforming constructor values and concrete function
! results as view actuals. Static calls on a concrete receiver reuse ordinary
! specialization next to dynamic dispatch through views.
program traits_runtime_numeric_03
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_03_provider_m
    implicit none
    type(Adder) :: adder_value
    type(Pairing) :: pairing_value
    type(Shape) :: shape_value
    type(Cell) :: cell_variable
    integer :: xi(5), n
    real(real64) :: xr(4)
    complex(real64) :: grid(3, 2)

    xi = [4, -1, 6, 2, 9]
    xr = [0.5_real64, 1.25_real64, -2.0_real64, 4.0_real64]
    grid = (1.0_real64, 0.0_real64)

    if (adder_value%sum(xi) /= 20) error stop 1
    if (runtime_sum(adder_value, xi(5:1:-2)) /= 19) error stop 2
    if (runtime_real_sum(adder_value, xr(2:4)) /= 3.25_real64) error stop 3

    pairing_value%bias = 1
    if (runtime_weigh(pairing_value, xi(1:3), 2) /= 20) error stop 4
    if (runtime_weigh(pairing_value, xi(1:3), -1) /= 10) error stop 5
    if (runtime_weigh_real(pairing_value, xr) /= 9.5_real64) error stop 6
    if (runtime_weigh_mixed(pairing_value, xi(2:4)) /= 16) error stop 7
    if (runtime_weigh_both(pairing_value, xr(1:2)) /= 5.5_real64) error stop 17

    if (runtime_cells(shape_value, grid) /= 302) error stop 8
    if (runtime_cells(shape_value, grid(1:3:2, :)) /= 202) error stop 9
    if (.not. runtime_positive(shape_value, xr(1:2))) error stop 10
    if (runtime_positive(shape_value, xr)) error stop 11
    if (runtime_mean(shape_value, xi) /= 6.5_real64) error stop 12

    if (observe(Cell()) /= 7) error stop 13
    if (observe(make_cell(3)) /= 7) error stop 14
    if (observe(cell_variable) /= 7) error stop 15
    call show(Cell(), n)
    if (n /= 8) error stop 16
    print '(a)', 'traits_runtime_numeric_03: ok'

contains

    integer function runtime_sum(summer, x)
        class(ISum), intent(in) :: summer
        integer,     intent(in) :: x(:)
        runtime_sum = summer%sum(x)
    end function runtime_sum

    real(real64) function runtime_real_sum(summer, x)
        class(ISum),  intent(in) :: summer
        real(real64), intent(in) :: x(:)
        runtime_real_sum = summer%sum{real(real64)}(x)
    end function runtime_real_sum

    integer function runtime_weigh(pair, x, w)
        class(IPair), intent(in) :: pair
        integer,      intent(in) :: x(:), w
        runtime_weigh = pair%weigh(x, w)
    end function runtime_weigh

    real(real64) function runtime_weigh_real(pair, x)
        class(IPair), intent(in) :: pair
        real(real64), intent(in) :: x(:)
        runtime_weigh_real = pair%weigh(x, 0.5_real64)
    end function runtime_weigh_real

    integer function runtime_weigh_mixed(pair, x)
        class(IPair), intent(in) :: pair
        integer,      intent(in) :: x(:)
        runtime_weigh_mixed = pair%weigh{integer, real(real64)}(x, 0.5_real64)
    end function runtime_weigh_mixed

    real(real64) function runtime_weigh_both(pair, x)
        class(IPair), intent(in) :: pair
        real(real64), intent(in) :: x(:)
        runtime_weigh_both = pair%weigh(x, 3)
    end function runtime_weigh_both

    integer function runtime_cells(shapes, x)
        class(IShape),   intent(in) :: shapes
        complex(real64), intent(in) :: x(:, :)
        runtime_cells = shapes%cells(x)
    end function runtime_cells

    logical function runtime_positive(shapes, x)
        class(IShape), intent(in) :: shapes
        real(real64),  intent(in) :: x(:)
        runtime_positive = shapes%positive(x)
    end function runtime_positive

    real(real64) function runtime_mean(shapes, x)
        class(IShape), intent(in) :: shapes
        integer,       intent(in) :: x(:)
        runtime_mean = shapes%mean(x)
    end function runtime_mean
end program traits_runtime_numeric_03
