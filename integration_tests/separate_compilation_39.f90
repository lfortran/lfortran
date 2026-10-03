program separate_compilation_39
    use separate_compilation_39a, only: recursive_t, noop
    implicit none

    type(recursive_t(kind(1.0))) :: x

    call noop()

    if (allocated(x%next)) error stop 1
end program separate_compilation_39
