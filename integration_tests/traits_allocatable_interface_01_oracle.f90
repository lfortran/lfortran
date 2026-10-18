module traits_allocatable_interface_01_oracle_m
    implicit none
    type, abstract :: ISetup
    contains
        procedure(fill_interface), deferred :: fill
    end type
    abstract interface
        subroutine fill_interface(self, data)
            import ISetup
            class(ISetup), intent(in) :: self
            real, allocatable, intent(out) :: data(:,:)
        end subroutine
    end interface
    type, extends(ISetup) :: Setup
        real :: value = 7.
    contains
        procedure :: fill
    end type
contains
    subroutine fill(self, data)
        class(Setup), intent(in) :: self
        real, allocatable, intent(out) :: data(:,:)
        if (allocated(data)) error stop 1
        allocate(data(2,3))
        data = self%value
    end subroutine
end module

program traits_allocatable_interface_01_oracle
    use traits_allocatable_interface_01_oracle_m
    implicit none
    type(Setup) :: source
    real, allocatable :: data(:,:)

    allocate(data(1,1))
    call source%fill(data)
    if (any(shape(data) /= [2,3]) .or. any(data /= 7.)) error stop 2
    source%value = 11.
    call source%fill(data)
    if (any(data /= 11.)) error stop 4
    deallocate(data)
end program
