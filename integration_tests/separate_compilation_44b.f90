module separate_compilation_44b_module
    use separate_compilation_44a_module
    implicit none

contains

    subroutine client()
        type(MyType) :: a, b
        a%val = 3.0d0
        b = a ** 2
        if (abs(b%val - 9.0d0) > 1.0d-12) error stop
        b = a ** 2.0d0
        if (abs(b%val - 9.0d0) > 1.0d-12) error stop
    end subroutine client

end module separate_compilation_44b_module
