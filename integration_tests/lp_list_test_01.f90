program lp_list_test_01
    integer :: x

    type(_lfortran_list_integer) :: test_list 
    call _lfortran_list_append(test_list, 1)
    call _lfortran_list_append(test_list, 10)
    x = _lfortran_list_len(test_list)
    if (x /= 2) error stop

    test_list = _lfortran_list_constant(1, 2, 3, 4)
    if (_lfortran_list_len(test_list) /= 4) error stop
    
    call _lfortran_list_append(test_list, -50)
    if (_lfortran_list_len(test_list) /= 5) error stop
    !
    type(_lfortran_list_real) :: test_list_r  = _lfortran_list_constant(1.0, 2.0, 3.0, 4.0)
    if (_lfortran_list_len(test_list_r) /= 4) error stop
    call _lfortran_list_append(test_list_r, 1.11)
    call _lfortran_list_append(test_list_r, -10.12)
    call _lfortran_list_append(test_list_r, 12.0)
    call _lfortran_list_append(test_list_r, -111.0)
    call _lfortran_list_append(test_list_r, -110.12)

    if (_lfortran_list_len(test_list_r) /= 9) error stop

    type(_lfortran_list_real) :: test_list_r1  
    if (_lfortran_list_len(test_list_r1) /= 0) error stop
    ! Add other intrinsics later
end program
