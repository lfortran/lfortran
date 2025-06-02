program lp_list_test_01
    implicit none
    integer :: x
    real :: eps = 1e-6

    type(_lfortran_list_integer) :: test_list 
    call _lfortran_list_append(test_list, 1)
    call _lfortran_list_append(test_list, 10)
    x = _lfortran_len(test_list)
    if (x /= 2) error stop

    if (_lfortran_get_item(test_list, 1) /= 10) error stop
    test_list = _lfortran_list_constant(1, 2, 3, 4)
    if (_lfortran_get_item(test_list, 1) /= 2) error stop
    if (_lfortran_get_item(test_list, 2) /= 3) error stop
    if (_lfortran_len(test_list) /= 4) error stop

    call _lfortran_set_item(test_list, 0, 10)
    if (_lfortran_get_item(test_list, 0) /= 10) error stop
    
    call _lfortran_list_append(test_list, -50)
    if (_lfortran_len(test_list) /= 5) error stop


    call _lfortran_list_reverse(test_list)
    if (_lfortran_len(test_list) /= 5) error stop
    if (_lfortran_get_item(test_list, 0) /= -50) error stop
    if (_lfortran_get_item(test_list, 3) /= 2) error stop
    if (_lfortran_get_item(test_list, 4) /= 10) error stop

    call _lfortran_list_append(test_list, 1)
    call _lfortran_list_append(test_list, 1)
    if (_lfortran_list_count(test_list, 1) /= 2) error stop
    if (_lfortran_list_count(test_list, -50) /= 1) error stop
     
    type(_lfortran_list_real) :: test_list_r  = _lfortran_list_constant(1.0, 2.0, 3.0, 4.0)
    if (_lfortran_len(test_list_r) /= 4) error stop
    call _lfortran_list_append(test_list_r, 1.11)
    call _lfortran_list_append(test_list_r, -10.12)
    call _lfortran_list_append(test_list_r, 12.0)
    call _lfortran_list_append(test_list_r, -111.0)
    call _lfortran_list_append(test_list_r, -110.12)

    if (_lfortran_len(test_list_r) /= 9) error stop
    if (abs(_lfortran_get_item(test_list_r, 5) + 10.12) > eps) error stop
    if (abs(_lfortran_get_item(test_list_r, 6) - 12.0) > eps) error stop

    call _lfortran_set_item(test_list_r, 3, 1212.33)
    if (abs(_lfortran_get_item(test_list_r, 3) - 1212.33) > eps) error stop

    type(_lfortran_list_real) :: test_list_r1  
    if (_lfortran_len(test_list_r1) /= 0) error stop

    ! Add other intrinsics later
end program
