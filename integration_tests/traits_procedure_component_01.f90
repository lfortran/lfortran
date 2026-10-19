module traits_procedure_component_01_m
    implicit none
    abstract interface :: IVal
        integer function get(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine add(self, n)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: n
        end subroutine
    end interface

    type, implements(IVal) :: Cell
        integer :: n = 0
    contains
        procedure :: get => cell_get
        procedure :: add => cell_add
    end type

    abstract interface
        subroutine visit_fn(v)
            import :: IVal
            class(IVal), intent(inout) :: v
        end subroutine
        integer function peek_fn(v)
            import :: IVal
            class(IVal), intent(in) :: v
        end function
    end interface

    type :: Driver
        integer :: step = 10
        procedure(visit_fn), pointer, nopass :: visit => null()
        procedure(peek_fn), pointer, nopass :: peek => null()
        procedure(bump_fn), pointer :: bump => null()
        procedure(stamp_fn), pointer, pass(self) :: stamp => null()
        class(IVal), allocatable :: item
    end type

    abstract interface
        subroutine bump_fn(self, v, n)
            import :: Driver, IVal
            class(Driver), intent(in) :: self
            class(IVal), intent(inout) :: v
            integer, intent(in) :: n
        end subroutine
        subroutine stamp_fn(v, self)
            import :: Driver, IVal
            class(IVal), intent(inout) :: v
            class(Driver), intent(in) :: self
        end subroutine
    end interface
contains
    integer function cell_get(self)
        class(Cell), intent(in) :: self
        cell_get = self%n
    end function

    subroutine cell_add(self, n)
        class(Cell), intent(inout) :: self
        integer, intent(in) :: n
        self%n = self%n + n
    end subroutine

    subroutine add1(v)
        class(IVal), intent(inout) :: v
        call v%add(1)
    end subroutine

    integer function getit(v)
        class(IVal), intent(in) :: v
        getit = v%get()
    end function

    subroutine bump_by(self, v, n)
        class(Driver), intent(in) :: self
        class(IVal), intent(inout) :: v
        integer, intent(in) :: n
        call v%add(self%step * n)
    end subroutine

    subroutine stamp_with(v, self)
        class(IVal), intent(inout) :: v
        class(Driver), intent(in) :: self
        call v%add(self%step + 100)
    end subroutine

    ! `v` is a trait view: the calls borrow it again.
    subroutine run_all(d, v)
        type(Driver), intent(in) :: d
        class(IVal), intent(inout) :: v
        call d%visit(v)
        call d%bump(v, 2)
        call d%stamp(v)
        if (d%peek(v) /= 131) error stop 1
    end subroutine
end module

program traits_procedure_component_01
    use traits_procedure_component_01_m
    implicit none
    type(Driver) :: d
    type(Cell) :: c
    class(IVal), allocatable :: o

    d%visit => add1
    d%peek => getit
    d%bump => bump_by
    d%stamp => stamp_with

    ! Concrete implementer actuals.
    call d%visit(c)
    if (c%n /= 1) error stop 2
    call d%bump(c, 3)
    if (c%n /= 31) error stop 3
    call d%bump(n=1, v=c)
    if (c%n /= 41) error stop 4
    call d%stamp(c)
    if (c%n /= 151) error stop 5
    if (d%peek(c) /= 151) error stop 6

    ! Owner actuals.
    o = Cell(5)
    call d%visit(o)
    call d%bump(o, 1)
    call d%stamp(o)
    if (o%get() /= 126) error stop 7
    if (d%peek(o) /= 126) error stop 8

    ! An owner component of the object whose component is called.
    d%item = Cell(1)
    call d%visit(d%item)
    call d%bump(d%item, 1)
    if (d%item%get() /= 12) error stop 9

    ! View actuals.
    c%n = 0
    call run_all(d, c)
    if (c%n /= 131) error stop 10
    print *, c%n, o%get(), d%item%get()
end program
