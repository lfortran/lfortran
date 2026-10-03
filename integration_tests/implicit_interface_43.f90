module implicit_interface_43_mod
    implicit none
    type :: runner
        procedure(), nopass, pointer :: caller => null()
    contains
        procedure, pass(this) :: set_caller
    end type
contains
    subroutine set_caller(this, caller)
        class(runner), intent(inout) :: this
        procedure() :: caller
        this%caller => caller
    end subroutine
end module

program implicit_interface_43
    use implicit_interface_43_mod
    implicit none
    type(runner) :: br
    call br%set_caller(upper_caller)
    
    ! Verification: check that the procedure pointer was set
    if (.not. associated(br%caller)) error stop
contains
    subroutine upper_caller(a)
        class(*), intent(in) :: a
    end subroutine
end program