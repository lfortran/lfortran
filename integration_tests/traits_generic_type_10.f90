module traits_generic_type_10_refs
    implicit none
    type :: Ref{T}
        type(T), pointer :: target
    end type
    type :: Box{T}
        type(T) :: item
    end type
end module

module traits_generic_type_10_subjects
    use traits_generic_type_10_refs
    implicit none
    type :: Subject
        type(Ref{Subject}) :: observer
        type(Box{Ref{Subject}}) :: nested
        type(Subject), pointer :: next => null()
        integer :: value = 7
    contains
        procedure :: increment
    end type
contains
    subroutine increment(self)
        class(Subject), intent(inout) :: self
        self%value = self%value + 1
    end subroutine
end module

program traits_generic_type_10
    use traits_generic_type_10_subjects
    implicit none
    type(Subject), target :: data, other
    data%observer%target => data
    data%nested%item%target => other
    data%next => other
    if (.not. associated(data%observer%target, data)) error stop 1
    if (.not. associated(data%nested%item%target, other)) error stop 2
    if (.not. associated(data%next, other)) error stop 3
    if (associated(other%next)) error stop 4
    call data%observer%target%increment()
    call data%nested%item%target%increment()
    call data%next%increment()
    if (data%value /= 8) error stop 5
    if (other%value /= 9) error stop 6
    data%observer%target%value = 42
    if (data%value /= 42) error stop 7
end program
