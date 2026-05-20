module procedure_pointer_23_mod
  implicit none
  abstract interface
    subroutine sub_i()
    end subroutine
  end interface
contains
  function get_ptr(fp) result(res)
    procedure(sub_i) :: fp
    procedure(sub_i), pointer :: res
    res => fp
  end function

  subroutine use_ptr(fp)
    procedure(sub_i), pointer, intent(in) :: fp
    call fp()
  end subroutine

  subroutine dosomething()
    integer :: x
    x = 42
    if (x /= 42) error stop
  end subroutine
end module

program procedure_pointer_23
  use procedure_pointer_23_mod
  implicit none
  call use_ptr(get_ptr(dosomething))
  print *, "ok"
end program
