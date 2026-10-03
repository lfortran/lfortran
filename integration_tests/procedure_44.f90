program procedure_44
  implicit none

  abstract interface
    real function f(x)
      real, value :: x
    end function
  end interface

  procedure(f), pointer :: fptr

  call test()
  if (associated(fptr)) error stop

contains

  subroutine test()
    fptr => null()
  end subroutine

end program
