! An associated type bound to character(len=5) by bindings whose messages end
! in explicit `integer(8), value, intent(in)` dummies: the shape of the
! hidden character lengths, which a binding of a character Element gets and
! its message does not. Each message and its binding have the same explicit
! arguments, before and after the hidden lengths are added.
module traits_associated_06_m
    implicit none

    abstract interface :: IStore
        type :: Element
        subroutine put(item, pos)
            type(Element), intent(in) :: item
            integer(8), value, intent(in) :: pos
        end subroutine put
        function matches(item, pos) result(found)
            type(Element), intent(in) :: item
            integer(8), value, intent(in) :: pos
            logical :: found
        end function matches
    end interface IStore

    type, sealed, implements(IStore) :: Names
        character(len=5) :: last = ''
        integer(8) :: at = 0
    contains
        procedure, pass :: put => names_put
        procedure, pass :: matches => names_matches
    end type Names

    type, sealed, implements(IStore) :: Counts
        integer :: last = 0
        integer(8) :: at = 0
    contains
        procedure, pass :: put => counts_put
        procedure, pass :: matches => counts_matches
    end type Counts

contains

    subroutine names_put(self, item, pos)
        type(Names), intent(inout) :: self
        character(len=5), intent(in) :: item
        integer(8), value, intent(in) :: pos
        self%last = item
        self%at = pos
    end subroutine names_put

    function names_matches(self, item, pos) result(found)
        type(Names), intent(in) :: self
        character(len=5), intent(in) :: item
        integer(8), value, intent(in) :: pos
        logical :: found
        found = self%last == item .and. self%at == pos
    end function names_matches

    subroutine counts_put(self, item, pos)
        type(Counts), intent(inout) :: self
        integer, intent(in) :: item
        integer(8), value, intent(in) :: pos
        self%last = item
        self%at = pos
    end subroutine counts_put

    function counts_matches(self, item, pos) result(found)
        type(Counts), intent(in) :: self
        integer, intent(in) :: item
        integer(8), value, intent(in) :: pos
        logical :: found
        found = self%last == item .and. self%at == pos
    end function counts_matches
end module traits_associated_06_m

program traits_associated_06
    use traits_associated_06_m, only: Names, Counts
    implicit none
    type(Names) :: n
    type(Counts) :: c
    integer :: checks

    checks = 0

    call n%put("John ", 3_8)
    call check(n%last == "John " .and. n%at == 3_8, 1)
    call check(n%matches("John ", 3_8) .and. .not. n%matches("Mary ", 3_8), 2)
    call check(.not. n%matches("John ", 4_8), 3)

    call c%put(42, 7_8)
    call check(c%last == 42 .and. c%at == 7_8, 4)
    call check(c%matches(42, 7_8) .and. .not. c%matches(41, 7_8), 5)

    print '(a,i0)', 'traits_associated_06 checks: ', checks
    if (checks /= 5) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_06 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_06
