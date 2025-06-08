program lp_set_test_01
    integer :: x

    type(_lfortran_set(integer)) :: test_set 
    test_set = _lfortran_set_constant(1, 2, 3, 4, 4, 5, 4, 10)
    call _lfortran_set_add(test_set, 1)
    call _lfortran_set_add(test_set, 10)
    call _lfortran_set_add(test_set, 11)
    x = _lfortran_len(test_set)
    if (x /= 7) error stop

    test_set = _lfortran_set_constant(1, 2, 3, 4)
    if (_lfortran_len(test_set) /= 4) error stop
    call _lfortran_set_add(test_set, -50)
    if (_lfortran_len(test_set) /= 5) error stop
     
    ! Add other intrinsics later
end program
