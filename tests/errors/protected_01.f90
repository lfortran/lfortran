module module_protected_01
  REAL, PROTECTED, save :: temp_c, temp_f

  CONTAINS

  SUBROUTINE set_temperature_c(c)
    REAL, INTENT(IN) :: c
    ! assignment to protected variable allowed here
    temp_c = c
    temp_f = temp_c*(9.0/5.0) + 32
  END SUBROUTINE
end module module_protected_01

program protected_01
  use module_protected_01

  call set_temperature_c(10.)
  ! assignment to protected variable not allowed here
  temp_c = 10
end program protected_01
