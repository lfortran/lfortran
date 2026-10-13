! Receiver effects of a universal trait (PAPER-D07): Count implements
! ICounter's bump with an INTENT(INOUT) receiver, which direct calls on
! concrete variables, components, array elements and INTENT(INOUT) dummies
! use to mutate them. Its read-only IValue conformance still serves runtime
! views and generic calls; a mutating conformance serves neither (see
! continue_compilation_traits_associated).
module traits_associated_03_contracts
    implicit none
    private
    public :: ICounter, IValue

    abstract interface :: ICounter
        subroutine bump(by)
            integer, intent(in) :: by
        end subroutine bump
    end interface ICounter

    abstract interface :: IValue
        function value() result(v)
            integer :: v
        end function value
    end interface IValue
end module traits_associated_03_contracts

module traits_associated_03_count_m
    use traits_associated_03_contracts, only: ICounter, IValue
    implicit none
    private

    type, public, sealed, implements(ICounter + IValue) :: Count
        integer :: n = 0
    contains
        procedure, pass :: bump => count_bump
        procedure, pass :: value => count_value
    end type Count

contains

    subroutine count_bump(self, by)
        type(Count), intent(inout) :: self
        integer,     intent(in)    :: by
        self%n = self%n + by
    end subroutine count_bump

    function count_value(self) result(v)
        type(Count), intent(in) :: self
        integer                 :: v
        v = self%n
    end function count_value
end module traits_associated_03_count_m

module traits_associated_03_use_m
    use traits_associated_03_contracts, only: IValue
    use traits_associated_03_count_m, only: Count
    implicit none
    private
    public :: Holder, bump_twice, view_value, generic_value

    type :: Holder
        type(Count) :: c
        type(Count) :: cs(3)
    end type Holder

contains

    subroutine bump_twice(c)
        type(Count), intent(inout) :: c
        call c%bump(1)
        call c%bump(1)
    end subroutine bump_twice

    function view_value(v) result(n)
        class(IValue), intent(in) :: v
        integer                   :: n
        n = v%value()
    end function view_value

    function generic_value{IValue :: T}(x) result(n)
        type(T), intent(in) :: x
        integer             :: n
        n = x%value()
    end function generic_value
end module traits_associated_03_use_m

program traits_associated_03
    use traits_associated_03_count_m, only: Count
    use traits_associated_03_use_m, only: Holder, bump_twice, view_value, generic_value
    implicit none
    integer :: checks, i
    type(Count) :: c
    type(Count) :: counts(4)
    type(Holder) :: h

    checks = 0

    call c%bump(5)
    call check(c%n == 5 .and. c%value() == 5, 1)
    call bump_twice(c)
    call check(c%n == 7, 2)
    call h%c%bump(2)
    call h%cs(2)%bump(3)
    call check(h%c%n == 2 .and. h%cs(2)%n == 3 .and. h%cs(1)%n == 0 .and. h%cs(3)%n == 0, 3)
    do i = 1, size(counts)
        call counts(i)%bump(i)
        call bump_twice(counts(i))
    end do
    call check(sum(counts%n) == 18 .and. counts(4)%n == 6, 4)
    call check(view_value(c) == 7 .and. view_value(h%cs(2)) == 3, 5)
    call check(generic_value(c) == 7 .and. generic_value(counts(1)) == 3, 6)

    print '(a,i0)', 'traits_associated_03 checks: ', checks
    if (checks /= 6) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_03 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_03
