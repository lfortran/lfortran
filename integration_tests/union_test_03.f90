module lp_union_test_03_mod
    implicit none
    _lfortran_union_type :: test_type
        integer                              :: int_
        real                                 :: float_
    end _lfortran_union_type
end module


program lp_union_test_03
    use lp_union_test_03_mod
    implicit none 
    integer::x
    type(test_type) :: test_union

    test_union%int_ = 100

    x = test_union%int_ !Support direct comparision later
    if ( x /= 100 ) error stop
end program

