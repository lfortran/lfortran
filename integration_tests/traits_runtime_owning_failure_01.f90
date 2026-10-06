module traits_runtime_owning_failure_01_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Payload
        integer, allocatable :: numbers(:)
        character(:), allocatable :: text
    end type
    implements IValue :: Payload
        procedure, pass :: value => read_payload
    end implements
contains
    function read_payload(self) result(r)
        class(Payload), intent(in) :: self
        integer :: r
        r = sum(self%numbers) + len(self%text)
    end function
end module

program traits_runtime_owning_failure_01
    use traits_runtime_owning_failure_01_m
    implicit none
    type(Payload) :: source
    type(Payload), allocatable :: absent
    class(IValue), allocatable :: owner
    integer :: count
    interface
        subroutine start_failures() bind(c)
        end subroutine
        integer function stop_failures() bind(c)
        end function
        integer function failure_mode() bind(c)
        end function
    end interface
    select case (failure_mode())
    case (1)
        allocate(owner, source=absent)
        error stop 1
    case (2)
        allocate(Payload :: owner)
        allocate(Payload :: owner)
        error stop 2
    case (3)
        deallocate(owner)
        error stop 3
    end select
    allocate(source%numbers(3))
    source%numbers = [1, 2, 3]
    source%text = "copy"
    call start_failures()
    allocate(owner, source=source)
    count = stop_failures()
    if (owner%value() /= 10) error stop 4
    if (count < 3) error stop 5
    deallocate(owner)
    if (allocated(owner)) error stop 6
    if (sum(source%numbers) /= 6 .or. source%text /= "copy") error stop 7
    deallocate(source%numbers, source%text)
    print *, "owning allocations:", count
end program
