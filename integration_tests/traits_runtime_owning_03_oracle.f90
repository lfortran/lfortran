module traits_runtime_owning_03_oracle_m
    implicit none
    integer :: finalized = 0, last_finalized = 0
    type, abstract :: Base
    end type
    type, extends(Base) :: Child
        integer :: n = 7
    contains
        final :: finish
    end type
    type :: Envelope
        class(Base), allocatable :: part
    end type
contains
    subroutine finish(self)
        type(Child), intent(inout) :: self
        finalized = finalized + 1
        last_finalized = self%n
    end subroutine
end module
program traits_runtime_owning_03_oracle
    use traits_runtime_owning_03_oracle_m
    implicit none
    type(Envelope) :: source, destination
    integer :: before
    allocate(Child :: source%part)
    allocate(Child :: destination%part)
    select type(part => destination%part)
    type is (Child)
        part%n = 99
    end select
    before = finalized
    destination = source
    if (finalized /= before + 1) error stop 1
    if (last_finalized /= 99) error stop 4
    select type(part => source%part)
    type is (Child)
        part%n = 41
    end select
    select type(part => destination%part)
    type is (Child)
        if (part%n /= 7) error stop 2
    end select
    before = finalized
    deallocate(source%part, destination%part)
    if (finalized /= before + 2) error stop 3
end program
