program get_environment_variable_01
  implicit none
  integer :: ilen
  integer :: stat
  character(30) :: ctmp
  character(30) :: str = "LFORTRAN_NONEXISTENT_VAR"

  ilen = len_trim(str)
  block
    call get_environment_variable(str(:ilen), ctmp, status=stat)
  end block
  if (stat == 0) error stop "Environment variable should not exist"
end program get_environment_variable_01