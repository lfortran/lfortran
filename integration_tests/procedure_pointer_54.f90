program procedure_pointer_54
    use procedure_pointer_54_mod
    implicit none
    type(t) :: a
    call set(a, impl)
    call run(a)
    call set(a)
    if (associated(a%f)) error stop 3
end program
