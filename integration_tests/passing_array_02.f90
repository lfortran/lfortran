MODULE passing_array_02_mod
    implicit none

    INTERFACE test_01_interface
       MODULE PROCEDURE test_01
    END INTERFACE test_01_interface

    INTERFACE test_02_interface
       MODULE PROCEDURE test_02
    END INTERFACE test_02_interface

    CONTAINS
 
    FUNCTION test_01 (len,value) result(res)
        implicit none
        INTEGER, intent(in) :: len 
        INTEGER, DIMENSION(3,len),intent(in) :: value 
        INTEGER :: res
        res = size(value)
    END FUNCTION test_01

    FUNCTION test_02 (value) result(res)
        implicit none
        ! Passed array has dimension = 30 while the array in this function has dimension = 3
        ! Slicing is premesible (3 <= 30), so accept it.  
        INTEGER, DIMENSION(3,1),intent(in) :: value 
        INTEGER :: res
        res = size(value)
    END FUNCTION test_02

    subroutine test_entry
        integer:: len
        INTEGER, DIMENSION(3, 10) :: arr
        INTEGER :: ret
        len = 10
        ret =  test_01_interface (len,arr)
        print * , ret
        if (ret /= 30) error stop
  
        ret =  test_01 (len,arr)
        print * , ret
        if (ret /= 30) error stop
  
        ret =  test_02_interface (arr)
        print * , ret
        if (ret /= 3) error stop
  
        ret =  test_02 (arr)
        print * , ret
        if (ret /= 3) error stop

    end subroutine test_entry
 
END MODULE passing_array_02_mod
program passing_array_02
    use passing_array_02_mod
    implicit none
    call test_entry
end program passing_array_02
