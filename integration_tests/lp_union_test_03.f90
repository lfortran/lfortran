module lp_union_test_03_mod
    implicit none
    _lfortran_union_type :: test_type
        integer                              :: int_
        real                                 :: float_
        _lfortran_list(integer)              :: list
    end _lfortran_union_type
end module


program lp_union_test_03
    use lp_union_test_03_mod
    implicit none 
    integer::x
    type(test_type) :: test_union

    test_union%list = _lfortran_list_constant(1, 1, 2, 4)
    call _lfortran_list_append(test_union%list, 10)

    if ( _lfortran_len(test_union%list) /= 5 ) error stop
    test_union%int_ = 100

    x = test_union%int_ !Support direct comparision later
    if ( x /= 100 ) error stop
end program

