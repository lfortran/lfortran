MODULE passing_array_04_mod
    implicit none

    CONTAINS
 
    FUNCTION test_01 (len,value) result(res)
        implicit none
        integer ,intent(in) :: len
        ! Accept passed array as it has dimension length not known at compile time.
        ! Run-time size (40000/12).
        INTEGER, DIMENSION(2,2,len),intent(in) :: value 
        INTEGER :: res
        res = size(value)
    END FUNCTION test_01

    subroutine test_entry
        integer :: len
        INTEGER, DIMENSION(6,2) :: value
        INTEGER :: ret
        len = 10000
        ret = test_01(len,value)
        print * , ret
        if(ret /= 40000) error stop 
    end subroutine test_entry
 
END MODULE passing_array_04_mod
program passing_array_04
    use passing_array_04_mod
    implicit none
    call test_entry
end program passing_array_04
