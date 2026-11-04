! A scalar can be assigned to an allocated runtime trait array; an array
! value and a nondefinable array are diagnosed.
module traits_array_assign_01_m
    implicit none
    abstract interface :: IValue
    end interface

    type, implements(IValue) :: Cell
    end type Cell
contains
    subroutine copy(arr, other)
        class(IValue), allocatable, intent(inout) :: arr(:)
        class(IValue), intent(in) :: other(:)
        arr = other
    end subroutine copy

    subroutine fill(arr)
        class(IValue), allocatable, intent(in) :: arr(:)
        arr = Cell()
    end subroutine fill
end module traits_array_assign_01_m
