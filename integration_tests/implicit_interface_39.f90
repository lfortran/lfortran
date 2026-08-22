module implicit_interface_39_mod
    implicit none
    integer :: val = 0
end module

subroutine camb()
    ! NEWPLOTC is declared EXTERNAL here but never called. Under
    ! --implicit-typing its name maps to INTEGER, so it used to be turned
    ! into an implicit integer function, colliding with the actual
    ! SUBROUTINE NEWPLOTC defined below and producing invalid LLVM IR.
    external newplotc
end subroutine

subroutine newplotc()
    use implicit_interface_39_mod
    val = 42
end subroutine

program implicit_interface_39
    use implicit_interface_39_mod
    implicit none
    call camb()
    call newplotc()
    if (val /= 42) error stop
end program
