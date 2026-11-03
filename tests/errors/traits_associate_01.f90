! The associate name of a runtime trait object is a borrowed view of a
! designator; arrays and expressions are not implemented yet.
module traits_associate_01_m
    implicit none
    abstract interface :: IValue
        integer function get(self)
            type(itself), pass, intent(in) :: self
        end function get
    end interface

    type, implements(IValue) :: Cell
        integer :: v = 0
    contains
        procedure :: get => cell_get
    end type Cell
contains
    integer function cell_get(self)
        class(Cell), intent(in) :: self
        cell_get = self%v
    end function cell_get

    function make() result(r)
        class(IValue), allocatable :: r
        allocate(Cell :: r)
    end function make

    subroutine arrays(vals)
        class(IValue), intent(in) :: vals(:)
        associate (all => vals)  ! {Error} associate names of runtime trait arrays are not implemented yet
        end associate
    end subroutine arrays

    subroutine parenthesized(v)
        class(IValue), intent(in) :: v
        associate (p => (v))  ! {Error} associate views of runtime trait object expressions are not implemented yet
        end associate
    end subroutine parenthesized

    subroutine result()
        associate (r => make())  ! {Error} associate views of runtime trait object expressions are not implemented yet
        end associate
    end subroutine result
end module traits_associate_01_m
