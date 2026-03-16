program pointer_association
  implicit none

  type :: box
    integer, pointer :: n => null()
  end type

  class(*), allocatable :: x
  type(box) :: b
  integer, pointer :: xp

  allocate(b%n)
  b%n = 1

  x = b  ! polymorphic assignment should preserve pointer association

  select type (x)
  type is (box)
    xp => x%n
  end select

  if (.not. associated(xp, b%n)) then
    print *, "FAIL: pointer not associated with original target"
    error stop 1
  else
    print *, "test passed"
  end if

  deallocate(b%n)
end program pointer_association