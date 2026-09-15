! A procedure with an implicit interface and a character result is passed
! to a dummy declared by an interface block and associated with a procedure
! pointer; the function result becomes an argument in the interfaces the
! calls go through.
module implicit_interface_84_mod
  implicit none
contains
  subroutine ii84_drv(f, r)
    interface
      character(len=8) function f(s)
        character(len=*) :: s
      end function
    end interface
    character(len=8), intent(out) :: r
    r = f("abc")
  end subroutine
end module

program implicit_interface_84
  use implicit_interface_84_mod
  implicit none
  character(len=8), external :: ii84_up8
  character(len=5), external :: ii84_c1, ii84_c2
  procedure(character(len=5)), pointer :: pc
  character(len=8) :: r
  character(len=10) :: acc
  integer :: i
  call ii84_drv(ii84_up8, r)
  if (r /= "abc") error stop 1
  acc = ""
  pc => ii84_c1
  do i = 1, 2
    acc = trim(acc) // pc(i)
    pc => ii84_c2
  end do
  if (acc /= "one11two22") error stop 2
  print *, r, acc
end program
