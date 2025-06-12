program dict_test_01
   implicit none
   real :: eps = 1e-6

   type(_lfortran_dict(integer, integer(4))):: dict_i
   dict_i = _lfortran_dict_constant(1, 2, 2, 5, 3, 7)
   if (_lfortran_len(dict_i) /= 3) error stop

   call _lfortran_set_item(dict_i, 3, 5)
   if (_lfortran_len(dict_i) /= 3) error stop

   call _lfortran_set_item(dict_i, -15, 6)
   if (_lfortran_len(dict_i) /= 4) error stop

   if (_lfortran_get_item(dict_i, -15) /= 6) error stop
   if (_lfortran_get_item(dict_i, 2) /= 5) error stop

   type(_lfortran_dict(integer(4), real(4))):: dict_r
   dict_r = _lfortran_dict_constant(1, 1.2, 2, 2.5, 3, 4.7)
   if (_lfortran_len(dict_r) /= 3) error stop

   call _lfortran_set_item(dict_r, 3, 5.5)
   if (_lfortran_len(dict_r) /= 3) error stop

   call _lfortran_set_item(dict_r, -14, 6.4)
   if (_lfortran_len(dict_r) /= 4) error stop

   if (abs(_lfortran_get_item(dict_i, -14) - 6.4) < eps) error stop
   if (abs(_lfortran_get_item(dict_i, 2) - 2.5) < eps) error stop

end program dict_test_01

