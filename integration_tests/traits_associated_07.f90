! Generic messages with a result of an associated type: each binding returns
! its implementation's one type, real(8) for Gauge and character(len=5) for
! Badge, whichever type parameter a call selects. The positive controls of
! the boundary that continue_compilation_traits_associated (A14) diagnoses.
module traits_associated_07_m
    implicit none

    abstract interface :: IValue
        function value() result(v)
            integer :: v
        end function value
    end interface IValue

    abstract interface :: IBox
        type :: Element
        subroutine put(x)
            type(Element), intent(in) :: x
        end subroutine put
        function pick{IValue :: T}(key) result(r)
            type(T), intent(in) :: key
            type(Element) :: r
        end function pick
    end interface IBox

    type :: Key
        integer :: k = 0
    end type Key

    type :: Other
        integer :: j = 0
    end type Other

    type :: Gauge
        real(8) :: last = 0
    end type Gauge

    type :: Badge
        character(len=5) :: last = 'abcde'
    end type Badge

    implements IValue :: Key
        procedure, pass :: value => key_value
    end implements

    implements IValue :: Other
        procedure, pass :: value => other_value
    end implements

    implements IBox :: Gauge
        procedure, pass :: put => gauge_put
        procedure, pass(self) :: pick => gauge_pick
    end implements

    implements IBox :: Badge
        procedure, pass :: put => badge_put
        procedure, pass(self) :: pick => badge_pick
    end implements

contains

    integer function key_value(self)
        class(Key), intent(in) :: self
        key_value = self%k
    end function key_value

    integer function other_value(self)
        class(Other), intent(in) :: self
        other_value = 10 * self%j
    end function other_value

    subroutine gauge_put(self, x)
        class(Gauge), intent(inout) :: self
        real(8), intent(in) :: x
        self%last = x
    end subroutine gauge_put

    function gauge_pick{IValue :: T}(key, self) result(r)
        type(T), intent(in) :: key
        class(Gauge), intent(in) :: self
        real(8) :: r
        r = self%last + key%value()
    end function gauge_pick

    subroutine badge_put(self, x)
        class(Badge), intent(inout) :: self
        character(len=5), intent(in) :: x
        self%last = x
    end subroutine badge_put

    function badge_pick{IValue :: T}(key, self) result(r)
        type(T), intent(in) :: key
        class(Badge), intent(in) :: self
        character(len=5) :: r
        r = self%last
        if (key%value() > 0) r(1:1) = 'X'
    end function badge_pick
end module traits_associated_07_m

program traits_associated_07
    use traits_associated_07_m, only: Key, Other, Gauge, Badge
    implicit none
    type(Gauge) :: g
    type(Badge) :: b
    integer :: checks

    checks = 0

    call g%put(1.5d0)
    call check(abs(g%pick(Key(2)) - 3.5d0) < 1d-12, 1)
    call check(abs(g%pick(Other(1)) - 11.5d0) < 1d-12, 2)

    call b%put('John ')
    call check(b%pick(Key(0)) == 'John ', 3)
    call check(b%pick(Other(1)) == 'Xohn ' .and. len(b%pick(Key(1))) == 5, 4)

    print '(a,i0)', 'traits_associated_07 checks: ', checks
    if (checks /= 4) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_07 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_07
