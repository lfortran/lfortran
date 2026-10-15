! Trait-object arguments that are not receivers, through generic code. A
! static generic procedure calls ICoeffs's messages on its binder and forwards
! its own view argument to them; an open runtime generic message whose binder
! ICoeffs constrains erases that binder to an ICoeffs view, whose operation
! wrappers forward the argument views of update and feed. Each argument keeps
! its payload address and implementation, by position or keyword.
module traits_runtime_view_argument_02_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    implicit none
    type(c_ptr) :: seen = c_null_ptr

    abstract interface :: IEstimates
        subroutine some_method(self)
            type(itself), pass, intent(inout) :: self
        end subroutine some_method
        integer function total(self)
            type(itself), pass, intent(in) :: self
        end function total
    end interface

    abstract interface :: ICoeffs
        subroutine update(self, estimates)
            type(itself), pass, intent(inout) :: self
            class(IEstimates), intent(inout) :: estimates
        end subroutine update
        subroutine feed(estimates, self)
            class(IEstimates), intent(inout) :: estimates
            type(itself), pass, intent(in) :: self
        end subroutine feed
    end interface

    abstract interface :: IRunner
        integer function run{ICoeffs :: T}(self, coeffs, x) result(r)
            type(itself), pass, intent(in) :: self
            type(T), intent(in) :: coeffs
            integer, intent(in) :: x
        end function run
    end interface

    type :: Tally
        integer :: n = 0
    end type Tally
    implements IEstimates :: Tally
        procedure, pass :: some_method => tally_step
        procedure, pass :: total => tally_total
    end implements

    type :: Coeffs
        integer :: updates = 0
    end type Coeffs
    implements ICoeffs :: Coeffs
        procedure, pass :: update => coeffs_update
        procedure, pass(self) :: feed => coeffs_feed
    end implements

    type :: Runner
        integer :: base = 0
    end type Runner
    implements IRunner :: Runner
        procedure, pass :: run => runner_run
    end implements
contains
    subroutine tally_step(self)
        class(Tally), target, intent(inout) :: self
        seen = c_loc(self%n)
        self%n = self%n + 1
    end subroutine tally_step

    integer function tally_total(self)
        class(Tally), intent(in) :: self
        tally_total = self%n
    end function tally_total

    subroutine coeffs_update(self, estimates)
        class(Coeffs), intent(inout) :: self
        class(IEstimates), intent(inout) :: estimates
        self%updates = self%updates + 1
        call estimates%some_method()
    end subroutine coeffs_update

    subroutine coeffs_feed(estimates, self)
        class(IEstimates), intent(inout) :: estimates
        class(Coeffs), intent(in) :: self
        call estimates%some_method()
    end subroutine coeffs_feed

    integer function runner_run{ICoeffs :: T}(self, coeffs, x) result(r)
        class(Runner), intent(in) :: self
        type(T), intent(in) :: coeffs
        integer, intent(in) :: x
        type(Tally) :: local
        local%n = x
        call coeffs%feed(local)
        call coeffs%feed(estimates=local)
        r = self%base + local%n
    end function runner_run

    subroutine drive{ICoeffs :: T}(coeffs, estimates)
        type(T), intent(inout) :: coeffs
        class(IEstimates), intent(inout) :: estimates
        call coeffs%update(estimates)
        call coeffs%feed(estimates=estimates)
    end subroutine drive
end module traits_runtime_view_argument_02_m

program traits_runtime_view_argument_02
    use iso_c_binding, only: c_loc, c_associated
    use traits_runtime_view_argument_02_m
    implicit none
    type(Coeffs) :: cc
    type(Tally), target :: t
    class(IEstimates), allocatable, target :: eo
    class(IRunner), allocatable :: r

    call drive(cc, t)
    if (cc%updates /= 1 .or. t%n /= 2) error stop 1
    if (.not. c_associated(seen, c_loc(t%n))) error stop 2
    allocate(eo, source=Tally(10))
    call drive(cc, eo)
    if (cc%updates /= 2 .or. eo%total() /= 12 .or. t%n /= 2) error stop 3
    select type (concrete => eo)
    type is (Tally)
        if (.not. c_associated(seen, c_loc(concrete%n))) error stop 4
    class default
        error stop 5
    end select
    allocate(r, source=Runner(1000))
    if (r%run(cc, 5) /= 1007) error stop 6
    if (r%run(x=1, coeffs=cc) /= 1003) error stop 7
    if (cc%updates /= 2) error stop 8
    print '(a)', 'traits_runtime_view_argument_02 ok'
end program traits_runtime_view_argument_02
