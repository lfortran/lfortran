module traits_runtime_inspection_state_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Cell
        integer :: n = 17
    end type
    type :: Other
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
    function empty() result(owner)
        class(IValue), allocatable :: owner
    end function
end module

program traits_runtime_inspection_state_01
    use traits_runtime_inspection_state_m
    implicit none
    class(IValue), allocatable :: owner
    class(IValue), pointer :: view => null()
    select case (command_argument_count())
    case (0)
        allocate(Cell :: owner)
        select type (concrete => owner)
        type is (Cell)
            if (concrete%n /= 17) error stop 1
        class default
            error stop 2
        end select
        deallocate(owner)
        stop
    case (1)
        select type (concrete => owner)
        class default
            print *, "invalid default reached"
        end select
    case (2)
        select type (concrete => view)
        class default
            print *, "invalid default reached"
        end select
    case (3)
        select type (concrete => empty())
        class default
            print *, "invalid default reached"
        end select
    case (4)
        select type (concrete => owner)
        type is (Other)
            print *, "invalid guard reached"
        end select
    case (5)
        select type (concrete => view)
        type is (Other)
            print *, "invalid guard reached"
        end select
    end select
    error stop 99
end program
