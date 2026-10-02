module finalization_10_module
    ! A reference counter with a defined assignment and a final subroutine,
    ! used from procedures of this module that the program calls. Their
    ! statements are read from the module file when the program is compiled.
    implicit none
    private
    public :: counter_t, object_t, reset_counts, ncreated, nassigned, nreleased

    type :: counter_t
        integer, pointer :: count => null()
    contains
        procedure :: assign_counter
        generic :: assignment(=) => assign_counter
        final :: release
    end type

    interface counter_t
        module procedure new_counter
    end interface

    type :: object_t
        type(counter_t) :: counter
    contains
        procedure :: start_counter
    end type

    interface object_t
        module procedure new_object
    end interface

    integer :: ncreated = 0, nassigned = 0, nreleased = 0

contains

    subroutine reset_counts()
        ncreated = 0
        nassigned = 0
        nreleased = 0
    end subroutine

    function new_counter() result(counter)
        type(counter_t) :: counter
        ncreated = ncreated + 1
        allocate(counter%count, source=1)
    end function

    subroutine assign_counter(lhs, rhs)
        class(counter_t), intent(inout) :: lhs
        class(counter_t), intent(in) :: rhs
        nassigned = nassigned + 1
        lhs%count => rhs%count
        if (associated(lhs%count)) lhs%count = lhs%count + 1
    end subroutine

    subroutine release(self)
        type(counter_t), intent(inout) :: self
        if (associated(self%count)) then
            nreleased = nreleased + 1
            self%count = self%count - 1
            if (self%count == 0) deallocate(self%count)
            nullify(self%count)
        end if
    end subroutine

    subroutine start_counter(self)
        class(object_t), intent(inout) :: self
        ! Defined assignment: new_counter is referenced once, and its result
        ! is finalized after the statement.
        self%counter = counter_t()
    end subroutine

    function new_object() result(object)
        type(object_t) :: object
        object%counter = counter_t()
    end function

end module
