! Type-bound GENERIC references select and call specifics whose passed
! object is a later dummy (PASS(arg)) or that take no passed object
! (NOPASS), with positional and keyword actuals, in subroutine and
! function references, including a result whose shape depends on another
! actual, and in generics that mix such specifics.
module generic_pass_01_m
    implicit none
    type :: Cell
        integer :: n = 0
    end type Cell
    type :: Visitor
        integer :: seen = 0
    contains
        procedure, pass(self) :: after_cell
        procedure, pass(self) :: after_scaled
        procedure, pass(self) :: count_after
        procedure, nopass :: count_fixed
        procedure :: visit_cell
        procedure, nopass :: note_value
        procedure, pass(self) :: repeat_after
        generic :: after => after_cell, after_scaled
        generic :: count => count_after, count_fixed
        generic :: mixed => visit_cell, note_value
        generic :: repeat => repeat_after
    end type Visitor
contains
    subroutine after_cell(item, self)
        type(Cell), intent(in) :: item
        class(Visitor), intent(inout) :: self
        self%seen = self%seen + item%n
    end subroutine after_cell

    subroutine after_scaled(item, scale, self, extra)
        type(Cell), intent(in) :: item
        integer, intent(in) :: scale
        class(Visitor), intent(inout) :: self
        integer, intent(in), optional :: extra
        self%seen = self%seen + scale * item%n
        if (present(extra)) self%seen = self%seen + extra
    end subroutine after_scaled

    integer function count_after(item, self)
        type(Cell), intent(in) :: item
        class(Visitor), intent(in) :: self
        count_after = self%seen + 10 * item%n
    end function count_after

    integer function count_fixed(k)
        integer, intent(in) :: k
        count_fixed = 1000 + k
    end function count_fixed

    subroutine visit_cell(self, item)
        class(Visitor), intent(inout) :: self
        type(Cell), intent(in) :: item
        self%seen = self%seen + 100 * item%n
    end subroutine visit_cell

    subroutine note_value(n)
        integer, intent(inout) :: n
        n = n + 1
    end subroutine note_value

    function repeat_after(item, self) result(r)
        type(Cell), intent(in) :: item
        class(Visitor), intent(in) :: self
        integer :: r(item%n)
        r = self%seen
    end function repeat_after
end module generic_pass_01_m

program generic_pass_01
    use generic_pass_01_m
    implicit none
    type(Visitor) :: v
    type(Cell) :: c
    integer :: n

    c%n = 1
    call v%after(c)
    if (v%seen /= 1) error stop 1
    call v%after(item=c)
    if (v%seen /= 2) error stop 2
    call v%after(c, 2)
    if (v%seen /= 4) error stop 3
    call v%after(scale=3, item=c, extra=5)
    if (v%seen /= 12) error stop 4
    if (v%count(c) /= 22) error stop 5
    if (v%count(item=c) /= 22) error stop 6
    if (v%count(4) /= 1004) error stop 7
    if (v%count(k=5) /= 1005) error stop 8
    call v%mixed(c)
    if (v%seen /= 112) error stop 9
    n = 7
    call v%mixed(n)
    if (n /= 8 .or. v%seen /= 112) error stop 10
    call v%mixed(n=n)
    if (n /= 9) error stop 11
    call v%mixed(item=c)
    if (v%seen /= 212) error stop 12
    c%n = 3
    if (size(v%repeat(c)) /= 3) error stop 13
    if (any(v%repeat(item=c) /= 212)) error stop 14
    print '(a)', 'generic_pass_01: passed objects placed'
end program generic_pass_01
