program lp_list_test_01
    integer :: x

    type(_lfortran_list_integer) :: test_list 
    call _lfortran_list_append(test_list, 1)
    call _lfortran_list_append(test_list, 10)
    x = _lfortran_list_len(test_list)
    if (x /= 2) error stop
    
    call _lfortran_list_append(test_list, -50)
    if (_lfortran_list_len(test_list) /= 3) error stop


    type(_lfortran_list_integer) :: test_list_r
    if (_lfortran_list_len(test_list_r) /= 0) error stop
    call _lfortran_list_append(test_list_r, 1.11)
    call _lfortran_list_append(test_list_r, -10.12)
    call _lfortran_list_append(test_list_r, 12)
    call _lfortran_list_append(test_list_r, -111)
    call _lfortran_list_append(test_list_r, -110.12)
    
    if (_lfortran_list_len(test_list_r) /= 5) error stop
    ! Add other intrinsics later
end program
