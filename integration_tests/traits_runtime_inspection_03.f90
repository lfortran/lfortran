module traits_runtime_inspection_03_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Cell
        integer :: n = 0
    contains
        final :: finish
    end type
    type :: Other
        integer :: n
    end type
    implements IValue :: Cell
        procedure, pass :: value => read_cell
    end implements
    integer :: calls = 0, finals = 0, total = 0
contains
    integer function read_cell(self)
        type(Cell), intent(in) :: self
        read_cell = self%n
    end function
    subroutine finish(self)
        type(Cell), intent(inout) :: self
        finals = finals + 1
        total = total + self%n
        self%n = -777
    end subroutine
    function make(n) result(owner)
        integer, intent(in) :: n
        class(IValue), allocatable :: owner
        calls = calls + 1
        allocate(Cell :: owner)
        select type (owner)
        type is (Cell)
            owner%n = n
        class default
            error stop 1
        end select
    end function
    subroutine returning()
        select type (concrete => make(37))
        type is (Cell)
            if (calls /= 6 .or. finals /= 5 .or. concrete%n /= 37) error stop 2
            return
        end select
        error stop 3
    end subroutine
end module

program traits_runtime_inspection_03
    use traits_runtime_inspection_03_m
    implicit none
    integer :: i
    procedure(make), pointer :: build
    select type (concrete => make(17))
    type is (Cell)
        if (calls /= 1 .or. finals /= 0 .or. concrete%n /= 17) error stop 4
    class default
        error stop 5
    end select
    if (finals /= 1 .or. total /= 17) error stop 6
    select type (concrete => make(19))
    type is (Other)
        error stop 7
    end select
    if (calls /= 2 .or. finals /= 2 .or. total /= 36) error stop 8
    select type (concrete => make(23))
    type is (Other)
        error stop 9
    class default
        if (calls /= 3 .or. finals /= 2 .or. concrete%value() /= 23) error stop 10
    end select
    if (finals /= 3 .or. total /= 59) error stop 11
    select type (outer => make(29))
    type is (Cell)
        select type (inner => make(31))
        class is (Cell)
            if (calls /= 5 .or. finals /= 3) error stop 12
            if (inner%n /= 31 .or. outer%n /= 29) error stop 13
        end select
        if (finals /= 4 .or. total /= 90 .or. outer%n /= 29) error stop 14
    end select
    if (finals /= 5 .or. total /= 119) error stop 15
    call returning()
    if (finals /= 6 .or. total /= 156) error stop 16
    outer_loop: do i = 1, 2
        select type (concrete => make(40 + i))
        type is (Cell)
            if (concrete%n /= 40 + i .or. finals /= 5 + i) error stop 17
            if (i == 1) cycle outer_loop
            exit outer_loop
        end select
    end do outer_loop
    if (calls /= 8 .or. finals /= 8 .or. total /= 239) error stop 18
    select type (concrete => make(43))
    type is (Cell)
        if (finals /= 8 .or. concrete%n /= 43) error stop 19
        go to 100
    end select
    error stop 20
100 if (finals /= 9 .or. total /= 282) error stop 21
    chosen: select type (concrete => make(47))
    type is (Cell)
        if (finals /= 9 .or. concrete%n /= 47) error stop 24
        exit chosen
    end select chosen
    if (calls /= 10 .or. finals /= 10 .or. total /= 329) error stop 25
    build => make
    select type (concrete => build(53))
    type is (Cell)
        if (calls /= 11 .or. finals /= 10 .or. concrete%n /= 53) error stop 22
    end select
    if (calls /= 11 .or. finals /= 11 .or. total /= 382) error stop 23
    print *, "inspection results:", calls, finals, total
end program
