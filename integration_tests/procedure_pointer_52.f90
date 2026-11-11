program procedure_pointer_52
    use procedure_pointer_52_mod
    implicit none
    type(t) :: x
    call set(x, impl)
    call x%f(1, 2, 3, 4, 5)
end program
