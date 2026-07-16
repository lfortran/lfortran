!Test: procedure pointer association between struct members with
! different interfaces (concrete vs generic procedure()).
! Verifies LLVM codegen bitcast when a typed procedure pointer
! is associated to a generic procedure() pointer slot.

module m_handler_153
    implicit none
    type :: handler
        procedure(), nopass, pointer :: callback => null()
    end type
end module

module m_dispatcher_153
    use m_handler_153
    implicit none

    type :: dispatcher
        procedure(), nopass, pointer :: callback => null()
    contains
        procedure, pass(this) :: set_callback
        procedure, pass(this) :: dispatch
    end type

contains

    subroutine set_callback(this, cb)
        class(dispatcher), intent(inout) :: this
        procedure() :: cb
        this%callback => cb
    end subroutine

    subroutine dispatch(this, a, f)
        class(dispatcher), intent(inout) :: this
        class(*), intent(in) :: a
        procedure() :: f
        type(handler) :: h

        ! This is the critical line: associating a typed procedure
        ! pointer (this%callback) to a generic procedure() slot
        ! (h%callback). Requires bitcast in LLVM codegen for typed
        ! pointers (LLVM < 15).
        if (associated(this%callback)) h%callback => this%callback
        if (.not. associated(h%callback)) error stop "h%callback should be associated"
    end subroutine

end module

program derived_types_153
    use m_dispatcher_153
    implicit none

    interface
        pure subroutine transform_str(str, res)
            character(*), intent(in)            :: str
            character(len(str)), intent(out)    :: res
        end subroutine
    end interface

    type(dispatcher) :: d

    ! Test 1: callback not set, should be null
    if (associated(d%callback)) error stop "callback should be null initially"

    ! Test 2: set callback and verify association
    call d%set_callback(my_callback)
    if (.not. associated(d%callback)) error stop "callback should be associated after set"

    ! Test 3: dispatch exercises the cross-struct pointer association
    call d%dispatch(42, my_callback)

    print *, "PASS"

contains
    subroutine my_callback(f, a)
        procedure(transform_str)    :: f
        class(*), intent(in)        :: a
    end subroutine
end program