MODULE passing_array_03_mod
    implicit none

    CONTAINS
 
    FUNCTION test_01 (value) result(res)
        implicit none
        ! Ranks aren't equal, but it works as long as we can slice the passed array.
        INTEGER, DIMENSION(2,2,2),intent(in) :: value 
        INTEGER :: res
        res = size(value)
    END FUNCTION test_01

    subroutine test_entry
        INTEGER, DIMENSION(6,2) :: value
        integer :: ret
        ret =  test_01(value)
        print *, ret
        if (ret /= 8) error stop
    end subroutine test_entry
 
END MODULE passing_array_03_mod
program passing_array_03
    use passing_array_03_mod
    implicit none
    call test_entry
end program passing_array_03
