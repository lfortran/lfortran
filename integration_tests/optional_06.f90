module optional_06_mod
  interface get_val
    module procedure :: get_val_float
    module procedure :: get_val_int
  end interface

contains
  subroutine get_val_float(val, stat)
    real, intent(in) :: val
    integer, intent(out), optional :: stat
  end subroutine
  subroutine get_val_int(val, default, stat)
    integer, intent(in) :: val
    integer, intent(in), optional :: default
    integer, intent(out), optional :: stat
    print *, present(default), present(stat)
    if (present(stat)) stat = 10
  end subroutine
end module

program optional_06
  use optional_06_mod
  integer :: stat = 0
  call get_val(3, stat=stat)
  if (stat /= 10) error stop 
end program optional_06