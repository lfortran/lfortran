module traits_optional_01_oracle_m
    implicit none
    type, abstract :: ICounter
    contains
        procedure(add_interface), deferred :: add
    end type
    abstract interface
        subroutine add_interface(self, increment)
            import ICounter
            class(ICounter), intent(inout) :: self
            integer, optional, intent(in) :: increment
        end subroutine
    end interface
    type, extends(ICounter) :: Counter
        integer :: value = 0
    contains
        procedure :: add
    end type
contains
    subroutine add(self, increment)
        class(Counter), intent(inout) :: self
        integer, optional, intent(in) :: increment
        if (present(increment)) then
            self%value = self%value + increment
        else
            self%value = self%value + 1
        end if
    end subroutine
end module

program traits_optional_01_oracle
    use traits_optional_01_oracle_m
    implicit none
    type(Counter) :: object
    call object%add()
    if (object%value /= 1) error stop 1
    call object%add(7)
    if (object%value /= 8) error stop 2
    call object%add(increment=3)
    if (object%value /= 11) error stop 3
end program
