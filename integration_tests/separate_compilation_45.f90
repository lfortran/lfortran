program separate_compilation_45
    use separate_compilation_45a_mod, only: greeter_t
    implicit none
    type(greeter_t) :: g
    g%id = 42
    call g%greet("hello")
    print *, "ok"
end program separate_compilation_45
