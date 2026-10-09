module traits_runtime_owning_11_oracle_m
    implicit none
    integer :: finals = 0, final_values = 0
    type :: Part
        integer :: n = 17
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
        final_values = final_values + self%n
        if (allocated(self%data)) then
            if (any(self%data /= 5)) error stop 10
        end if
    end subroutine
end module

program traits_runtime_owning_11_oracle
    use traits_runtime_owning_11_oracle_m
    implicit none
    type(Envelope) :: source, empty
    type(Envelope), allocatable :: owner
    integer :: i
    allocate(source%parts(2))
    do i = 1, 2
        allocate(source%parts(i)%data(3))
        source%parts(i)%data = 5
    end do
    allocate(owner, source=source)
    if (finals /= 0) error stop 1
    owner = empty
    if (finals /= 2 .or. final_values /= 34) error stop 2
    if (allocated(owner%parts)) error stop 3
    owner = source
    if (finals /= 2 .or. sum(owner%parts%n) /= 34) error stop 4
    owner = empty
    if (finals /= 4 .or. final_values /= 68) error stop 5
    deallocate(owner, source%parts)
    if (finals /= 6 .or. final_values /= 102) error stop 6
end program
