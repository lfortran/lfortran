MODULE input_module
    implicit none
    INTERFACE test_interface
       MODULE PROCEDURE test_01, test_01, test_01
    END INTERFACE test_interface
CONTAINS

    SUBROUTINE test_01 (x)
        implicit none
        integer , intent(in):: x
        print *, x
    END SUBROUTINE test_01

END MODULE input_module


PROGRAM main
    USE input_module
    CALL test_interface(1)
END PROGRAM main
