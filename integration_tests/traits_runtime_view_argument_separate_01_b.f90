! The second provider: same-named Coeffs and Tally types, implemented with
! passed objects named me and it in the last position; its Tally counts by ten.
module traits_runtime_view_argument_separate_01_b_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    use traits_runtime_view_argument_separate_01_estimates_m, only: IEstimates
    use traits_runtime_view_argument_separate_01_contracts_m, only: ICoeffs
    implicit none
    type(c_ptr) :: b_seen = c_null_ptr

    type :: Coeffs
        integer :: updates = 0
    end type Coeffs
    implements ICoeffs :: Coeffs
        procedure, pass(me) :: blend => b_blend
        procedure, pass(me) :: update => b_update
    end implements

    type :: Tally
        integer :: n = 0
    end type Tally
    implements IEstimates :: Tally
        procedure, pass(it) :: total => b_total
        procedure, pass(it) :: some_method => b_step
    end implements
contains
    subroutine b_update(estimates, me)
        class(IEstimates), intent(inout) :: estimates
        class(Coeffs), intent(inout) :: me
        me%updates = me%updates + 1
        call estimates%some_method()
    end subroutine b_update

    integer function b_blend(estimates, weight, me)
        class(IEstimates), intent(inout) :: estimates
        integer, intent(in) :: weight
        class(Coeffs), intent(in) :: me
        b_blend = weight + estimates%total() + 1000 * me%updates
        call estimates%some_method()
    end function b_blend

    subroutine b_step(it)
        class(Tally), target, intent(inout) :: it
        b_seen = c_loc(it%n)
        it%n = it%n + 10
    end subroutine b_step

    integer function b_total(it)
        class(Tally), intent(in) :: it
        b_total = it%n
    end function b_total
end module traits_runtime_view_argument_separate_01_b_m
