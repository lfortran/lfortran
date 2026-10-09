module traits_type_adoption_04_provider
    implicit none
    integer :: final_count = 0, final_sum = 0
    integer :: receiver_finals = 0

    abstract interface :: IValue
        integer function value()
        end function
    end interface

    type :: Marker
        integer :: stamp = 0
    contains
        final :: finish_marker
    end type

    type :: Parent
        integer :: n = 17
    contains
        procedure :: value => parent_value
        procedure, pass(self) :: named => parent_named
        procedure, pass(self) :: add => parent_add
    end type

    type, extends(Parent) :: Open
    contains
        procedure :: value => open_value
    end type

    type, extends(Parent), sealed, implements(IValue) :: Closed
        integer :: bias = 2
    contains
        procedure :: value => closed_value
        procedure, pass(self) :: named => closed_named
        procedure, pass(self) :: add => closed_add
        final :: finish_closed
    end type
contains
    subroutine finish_marker(self)
        type(Marker), intent(inout) :: self
        final_count = final_count + 1
        final_sum = final_sum + self%stamp
    end subroutine

    subroutine finish_closed(self)
        type(Closed), intent(inout) :: self
        receiver_finals = receiver_finals + 1
    end subroutine

    integer function parent_value(self) result(n)
        class(Parent), intent(in) :: self
        n = self%n
    end function

    integer function open_value(self) result(n)
        class(Open), intent(in) :: self
        n = self%n + 1
    end function

    integer function closed_value(self) result(n)
        type(Closed), intent(in) :: self
        type(Marker) :: local
        local%stamp = 7
        n = self%n + self%bias
    end function

    pure integer function parent_named(scale, self, offset) result(n)
        integer, intent(in) :: scale
        class(Parent), intent(in) :: self
        integer, optional, intent(in) :: offset
        n = scale * self%n
        if (present(offset)) n = n + offset
    end function

    pure integer function closed_named(scale, self, offset) result(n)
        integer, intent(in) :: scale
        type(Closed), intent(in) :: self
        integer, optional, intent(in) :: offset
        n = scale * self%n + self%bias
        if (present(offset)) n = n + offset
    end function

    subroutine parent_add(step, self)
        integer, intent(in) :: step
        class(Parent), intent(inout) :: self
        self%n = self%n + step
    end subroutine

    subroutine closed_add(step, self)
        integer, intent(in) :: step
        type(Closed), intent(inout) :: self
        self%n = self%n + step + self%bias
    end subroutine

    integer function read_static{IValue :: T}(item) result(n)
        type(T), intent(in) :: item
        n = item%value()
    end function

    integer function read_runtime(item) result(n)
        class(IValue), intent(in) :: item
        n = item%value()
    end function
end module
