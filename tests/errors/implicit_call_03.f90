! Test: keyword argument in a call to a variable with --implicit-interface
! The variable becomes a procedure with an implicit interface, which cannot
! take keyword arguments, so this is a semantic error.
program implicit_call_03
    implicit none
    integer :: i
    call i(a=1)
end program
