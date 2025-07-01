module lp_union_test_02_mod
    implicit none
    
    _lfortran_union_type :: test_type
        integer :: x
        real    :: y
    end _lfortran_union_type


    _lfortran_union_type :: test_type1
        integer            :: x
        integer(kind=8)    :: y
    end _lfortran_union_type
end module


program lp_union_test_01
    use lp_union_test_02_mod
    implicit none 
    real             ::eps = 1e-6, x
    integer          :: y 
    integer(kind=8)  :: z
    type(test_type)  :: test_union
    type(test_type1) :: test_union1

    ! Memory value of pi
    test_union%x = 1078530011 

    ! Doesn't currently support direct operations
    x = test_union%y
    if ( abs(x - 3.141593) > eps ) error stop


    test_union1%x = 121229102
    y = test_union1%x
    z = test_union1%y

    ! TODO: FIX

    !if ( y /= int(z) ) error stop

end program

