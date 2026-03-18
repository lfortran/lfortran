program separate_compilation_45
    use separate_compilation_45a_mod, only: greeter_t
    implicit none
    type(greeter_t) :: g
    g%id = 42
    if (g%id /= 42) error stop "expected greeter id"
    call g%greet("hello")
    if (g%id /= 42) error stop "unexpected greeter id after greet"
end program separate_compilation_45
