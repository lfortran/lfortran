program declaration_07
  implicit none
  type t
    integer :: i
  end type

  interface
    subroutine foo()
    end subroutine
  end interface

  type(t) :: x = foo
end program
