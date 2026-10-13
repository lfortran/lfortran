! Standard-Fortran oracle of traits_associated_06: the conformances are
! ordinary type-bound procedures.
module traits_associated_06_oracle_m
    implicit none

    type :: Names
        character(len=5) :: last = ''
        integer(8) :: at = 0
    contains
        procedure, pass :: put => names_put
        procedure, pass :: matches => names_matches
    end type Names

    type :: Counts
        integer :: last = 0
        integer(8) :: at = 0
    contains
        procedure, pass :: put => counts_put
        procedure, pass :: matches => counts_matches
    end type Counts

contains

    subroutine names_put(self, item, pos)
        class(Names), intent(inout) :: self
        character(len=5), intent(in) :: item
        integer(8), value, intent(in) :: pos
        self%last = item
        self%at = pos
    end subroutine names_put

    function names_matches(self, item, pos) result(found)
        class(Names), intent(in) :: self
        character(len=5), intent(in) :: item
        integer(8), value, intent(in) :: pos
        logical :: found
        found = self%last == item .and. self%at == pos
    end function names_matches

    subroutine counts_put(self, item, pos)
        class(Counts), intent(inout) :: self
        integer, intent(in) :: item
        integer(8), value, intent(in) :: pos
        self%last = item
        self%at = pos
    end subroutine counts_put

    function counts_matches(self, item, pos) result(found)
        class(Counts), intent(in) :: self
        integer, intent(in) :: item
        integer(8), value, intent(in) :: pos
        logical :: found
        found = self%last == item .and. self%at == pos
    end function counts_matches
end module traits_associated_06_oracle_m

program traits_associated_06_oracle
    use traits_associated_06_oracle_m, only: Names, Counts
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

    print '(a,i0)', 'traits_associated_06_oracle checks: ', checks
    if (checks /= 5) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_06_oracle failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_06_oracle
