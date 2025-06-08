module input_module
    implicit none
    interface test_interface
       module procedure test_01, test_01, test_01
    end interface test_interface
contains

    subroutine test_01 (x)
        implicit none
        integer , intent(in):: x
        print *, x
    end subroutine test_01

end module input_module


program main
    use input_module
    call test_interface(1)
end program main