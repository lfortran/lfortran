! Standard-Fortran oracle of traits_associated_03: Count's bump and value
! are ordinary type-bound procedures; bump mutates its INTENT(INOUT)
! receiver through direct calls on variables, components, array elements
! and an INTENT(INOUT) dummy. The runtime view and the generic call of the
! read-only value become a polymorphic dummy and an ordinary call.
module traits_associated_03_oracle_count_m
    implicit none
    private

    type, public :: Count
        integer :: n = 0
    contains
        procedure, pass :: bump => count_bump
        procedure, pass :: value => count_value
    end type Count

contains

    subroutine count_bump(self, by)
        class(Count), intent(inout) :: self
        integer,      intent(in)    :: by
        self%n = self%n + by
    end subroutine count_bump

    function count_value(self) result(v)
        class(Count), intent(in) :: self
        integer                  :: v
        v = self%n
    end function count_value
end module traits_associated_03_oracle_count_m

module traits_associated_03_oracle_use_m
    use traits_associated_03_oracle_count_m, only: Count
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
        class(Count), intent(in) :: v
        integer                  :: n
        n = v%value()
    end function view_value

    function generic_value(x) result(n)
        type(Count), intent(in) :: x
        integer                 :: n
        n = x%value()
    end function generic_value
end module traits_associated_03_oracle_use_m

program traits_associated_03_oracle
    use traits_associated_03_oracle_count_m, only: Count
    use traits_associated_03_oracle_use_m, only: Holder, bump_twice, view_value, &
        generic_value
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
end program traits_associated_03_oracle
