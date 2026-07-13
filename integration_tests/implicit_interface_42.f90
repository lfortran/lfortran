module implicit_interface_42_mod
contains
    subroutine sub_a(caller)
        procedure() :: caller
    end subroutine
end module

program implicit_interface_42
    use implicit_interface_42_mod
    
    type :: runner
        procedure(), nopass, pointer :: caller => null()
    end type

    type(runner) :: r
    
    ! Verification: check that the procedure pointer is initially null
    if (associated(r%caller)) error stop
end program
