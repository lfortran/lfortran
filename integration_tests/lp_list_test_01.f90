program lp_list_test_01

    ! placeholder integer to add entry to ptr_type
    integer :: _placeholder = 15

    type(_lfortran_list_integer) :: test_list 
    call _lfortran_list_append(test_list, 1)
    call _lfortran_list_append(test_list, 10)
    if (_lfortran_list_len(test_list) /= 2) error stop
    
    call _lfortran_list_append(test_list, -50)
    if (_lfortran_list_len(test_list) /= 3) error stop
    
    ! Add other intrinsics later
end program
