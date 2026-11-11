program procedure_pointer_53
    use procedure_pointer_53_mod
    implicit none
    type(t) :: x
    call set(x, impl)
    call x%f(1, 2)
    call set(x)
    if (associated(x%f)) error stop 3
end program
