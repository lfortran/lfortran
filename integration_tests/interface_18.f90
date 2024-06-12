MODULE input_module
    implicit none
    INTERFACE test_01_interface
       MODULE PROCEDURE test_01
    END INTERFACE test_01_interface

    INTERFACE test_02_interface
       MODULE PROCEDURE test_02
    END INTERFACE test_02_interface

    INTERFACE test_03_interface
       MODULE PROCEDURE test_03
    END INTERFACE test_03_interface

 CONTAINS

    pure FUNCTION func() result (args2)
        integer ::  args2 
        args2 = 10
    end FUNCTION func

    SUBROUTINE test_01 (ilen,value )
        implicit none
        integer  ,intent(in):: ilen 
        INTEGER, DIMENSION(func()),intent(in) :: value
        print *,size(value)
    END SUBROUTINE test_01

    SUBROUTINE test_02 (ilen,value )
        implicit none
        integer  ,intent(in):: ilen 
        INTEGER, DIMENSION(ilen),intent(in) :: value
        print *,size(value)
    END SUBROUTINE test_02

    SUBROUTINE test_03 (ilen,value)
        implicit none
        integer  ,intent(in):: ilen 
        INTEGER, DIMENSION(ilen + func() + 10),intent(in) :: value
        print *,size(value)
    END SUBROUTINE test_03


    SUBROUTINE input_var_bcast
        implicit none
       INTEGER :: ilen = 20
       INTEGER, DIMENSION(6) :: ipak
       ipak = [1,2,3,4,5,6]
       CALL test_01_interface (ilen,ipak)
       CALL test_02_interface (ilen,ipak)
       CALL test_03_interface (ilen,ipak)
    END SUBROUTINE input_var_bcast



 END MODULE input_module

 program name
    use input_module
    implicit none
    call input_var_bcast
 end program name 