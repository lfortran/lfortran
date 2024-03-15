module test_module
    implicit none

    type :: test_type
        real :: test_real_value
    contains
        procedure, nopass :: test_subroutine
    end type test_type

contains
    subroutine test_subroutine(x)
        integer, optional, intent(in) :: x
        print *, "Hello World"
    end subroutine test_subroutine
end module test_module

program main
    use test_module
    implicit none

    type(test_type) :: test_type_instance

    call test_type_instance%test_subroutine()

end program main
