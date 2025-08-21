program lp_list_of_lists_test
    implicit none
    type(_lfortran_list(_lfortran_list(integer))) :: x
    type(_lfortran_list(integer)) :: y 
    
    call _lfortran_list_append(x, _lfortran_list_constant(1, 0, 1, 12, 15, 15))
    y = _lfortran_list_constant(10, 10, 10)
    call _lfortran_list_append(y, 16)
    call _lfortran_list_append(x, y)

    if (_lfortran_get_item(_lfortran_get_item(x, 0), 1) /= 0) error stop
    if (_lfortran_get_item(_lfortran_get_item(x, 1), 0) /= 10) error stop


    if (_lfortran_len(_lfortran_get_item(x, 0)) /= 6) error stop
    if (_lfortran_len(_lfortran_get_item(x, 1)) /= 4) error stop

    ! Making sure its copy
    call _lfortran_list_append(y, -22)
    if (_lfortran_len(_lfortran_get_item(x, 1)) /= 4) error stop

    call _lfortran_list_append(_lfortran_get_item(x, 1), 36)
    if (_lfortran_len(_lfortran_get_item(x, 1)) /= 5) error stop
end program lp_list_of_lists_test

