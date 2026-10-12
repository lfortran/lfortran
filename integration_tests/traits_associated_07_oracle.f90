! Standard-Fortran oracle of traits_associated_07: each generic binding is
! written out for the key types it is called with.
module traits_associated_07_oracle_m
    implicit none

    type :: Key
        integer :: k = 0
    end type Key

    type :: Other
        integer :: j = 0
    end type Other

    type :: Gauge
        real(8) :: last = 0
    contains
        procedure :: put => gauge_put
        procedure :: pick_key => gauge_pick_key
        procedure :: pick_other => gauge_pick_other
    end type Gauge

    type :: Badge
        character(len=5) :: last = 'abcde'
    contains
        procedure :: put => badge_put
        procedure :: pick_key => badge_pick_key
        procedure :: pick_other => badge_pick_other
    end type Badge

contains

    integer function key_value(which)
        type(Key), intent(in) :: which
        key_value = which%k
    end function key_value

    integer function other_value(which)
        type(Other), intent(in) :: which
        other_value = 10 * which%j
    end function other_value

    subroutine gauge_put(self, x)
        class(Gauge), intent(inout) :: self
        real(8), intent(in) :: x
        self%last = x
    end subroutine gauge_put

    function gauge_pick_key(self, which) result(r)
        class(Gauge), intent(in) :: self
        type(Key), intent(in) :: which
        real(8) :: r
        r = self%last + key_value(which)
    end function gauge_pick_key

    function gauge_pick_other(self, which) result(r)
        class(Gauge), intent(in) :: self
        type(Other), intent(in) :: which
        real(8) :: r
        r = self%last + other_value(which)
    end function gauge_pick_other

    subroutine badge_put(self, x)
        class(Badge), intent(inout) :: self
        character(len=5), intent(in) :: x
        self%last = x
    end subroutine badge_put

    function badge_pick_key(self, which) result(r)
        class(Badge), intent(in) :: self
        type(Key), intent(in) :: which
        character(len=5) :: r
        r = self%last
        if (key_value(which) > 0) r(1:1) = 'X'
    end function badge_pick_key

    function badge_pick_other(self, which) result(r)
        class(Badge), intent(in) :: self
        type(Other), intent(in) :: which
        character(len=5) :: r
        r = self%last
        if (other_value(which) > 0) r(1:1) = 'X'
    end function badge_pick_other
end module traits_associated_07_oracle_m

program traits_associated_07_oracle
    use traits_associated_07_oracle_m, only: Key, Other, Gauge, Badge
    implicit none
    type(Gauge) :: g
    type(Badge) :: b
    integer :: checks

    checks = 0

    call g%put(1.5d0)
    call check(abs(g%pick_key(Key(2)) - 3.5d0) < 1d-12, 1)
    call check(abs(g%pick_other(Other(1)) - 11.5d0) < 1d-12, 2)

    call b%put('John ')
    call check(b%pick_key(Key(0)) == 'John ', 3)
    call check(b%pick_other(Other(1)) == 'Xohn ' .and. len(b%pick_key(Key(1))) == 5, 4)

    print '(a,i0)', 'traits_associated_07_oracle checks: ', checks
    if (checks /= 4) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_07_oracle failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_07_oracle
