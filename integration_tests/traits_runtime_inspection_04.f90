module traits_runtime_inspection_04_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: KindBox(k)
        integer, kind :: k = 4
        integer :: n
    end type
    type :: Cell
        integer :: n
    end type
    implements IValue :: Cell
        procedure, pass :: value => read_cell
    end implements
contains
    integer function read_cell(self)
        type(Cell), intent(in) :: self
        read_cell = self%n
    end function
    subroutine inspect(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        type is (KindBox(4))
            error stop 1
        type is (KindBox(8))
            error stop 2
        type is (Cell)
            if (concrete%n /= 17) error stop 3
        class default
            error stop 4
        end select
        if (view%value() /= 17) error stop 5
    end subroutine
end module

program traits_runtime_inspection_04
    use traits_runtime_inspection_04_m
    implicit none
    type(KindBox(4)), target :: first
    type(KindBox(8)), target :: second
    type(Cell), target :: object
    class(IValue), pointer :: view
    object%n = 17
    first%n = 23
    second%n = 29
    if (storage_size(first) /= storage_size(second)) error stop 6
    view => object
    call inspect(view)
    nullify(view)
end program
