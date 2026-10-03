program transfer_26
  implicit none
  type :: a_t
    integer, pointer :: p
  end type
  type :: b_t
    integer, pointer :: p
  end type
  type(a_t) :: a
  type(b_t) :: b
  integer, target :: val

  val = 42
  a%p => val
  b = transfer(a, b)
  if (.not. associated(b%p)) error stop
  if (b%p /= 42) error stop
  print *, "PASS"
end program
