module tuple_test_04_mod
   implicit none
   real :: eps = 1e-12

contains
   subroutine tests()
      _lfortran_tuple(_lfortran_list(integer), character(len=:)) :: ttype
      _lfortran_tuple(_lfortran_list(integer), character(len=:)) :: contents

      ttype = _lfortran_tuple_constant(_lfortran_list_constant(-1), "dimensions")
      contents = _lfortran_tuple_constant(_lfortran_list_constant(1, 2), "")

      ! TODO: Add List cmp
      if (_lfortran_len(_lfortran_get_item(ttype, 0)) /= 1) error stop
      if (len(_lfortran_get_item(contents, 1)) /= 0) error stop
      if (_lfortran_get_item(ttype, 1) /= "dimensions") error stop
   end subroutine
end module

program run_tuples
   use tuple_test_04_mod
   call tests()
end program
