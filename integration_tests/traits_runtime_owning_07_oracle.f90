module traits_runtime_owning_07_oracle_m
    implicit none
    integer :: finals = 0, finalized_values = 0
    type :: Part
        integer :: n = 7
        integer, allocatable :: data(:)
    contains
        final :: finish
    end type
    type :: Envelope
        type(Part), allocatable :: parts(:)
    end type
contains
    impure elemental subroutine finish(self)
        type(Part), intent(inout) :: self
        finals = finals + 1
        finalized_values = finalized_values + self%n
    end subroutine
end module

program traits_runtime_owning_07_oracle
    use traits_runtime_owning_07_oracle_m
    implicit none
    type(Envelope) :: source
    type(Envelope), allocatable :: owner
    integer :: i

    allocate(source%parts(2))
    source%parts%n = 17
    do i = 1, 2
        allocate(source%parts(i)%data(1))
        source%parts(i)%data = 5
    end do
    allocate(owner, source=source)
    if (finals /= 0 .or. sum(owner%parts%n) /= 34) error stop 1
    source%parts%n = 23
    source%parts(1)%data = 99
    if (sum(owner%parts%n) /= 34) error stop 2
    if (owner%parts(1)%data(1) /= 5) error stop 3
    deallocate(owner)
    if (finals /= 2 .or. finalized_values /= 34) error stop 4
    deallocate(source%parts)
    if (finals /= 4 .or. finalized_values /= 80) error stop 5
end program
