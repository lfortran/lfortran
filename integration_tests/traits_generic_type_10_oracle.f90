module traits_generic_type_10_oracle_subjects
    implicit none
    type :: Ref_Subject
        type(Subject), pointer :: target
    end type
    type :: Box_Ref_Subject
        type(Ref_Subject) :: item
    end type
    type :: Subject
        type(Ref_Subject) :: observer
        type(Box_Ref_Subject) :: nested
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

program traits_generic_type_10_oracle
    use traits_generic_type_10_oracle_subjects
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
