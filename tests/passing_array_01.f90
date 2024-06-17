MODULE passing_array_01_mod
    implicit none

    INTERFACE test_01_interface
       MODULE PROCEDURE test_01
    END INTERFACE test_01_interface

    CONTAINS
 
    FUNCTION test_01 (len,value) result(res)
        implicit none
        integer :: len 
        INTEGER, DIMENSION(3,len,len),intent(in) :: value 
        INTEGER :: res
        res = size(value)
    END FUNCTION test_01

    subroutine test_entry
        integer:: len 
        INTEGER, DIMENSION(6,2) :: value
        INTEGER :: ret
        len = 1
        ret =  test_01(len,value) ! This works with no problems.
        ret =  test_01_interface(len,value) ! This raise error as interfaces are strict on matching the number of ranks, so it doesn't find a match.
    end subroutine test_entry
 
END MODULE passing_array_01_mod

program passing_array_01
    use passing_array_01_mod
    implicit none
    call test_entry
end program passing_array_01
