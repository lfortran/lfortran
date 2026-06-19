program main
  implicit none
  integer, target :: a(2)

  call take_pointer_rank(a)
  print *, "ok"
contains
  subroutine take_pointer_rank(x)
    integer, pointer, intent(in) :: x(..)

    ! Accessing rank forces the backend to read the descriptor.
    ! This triggers the ICE on main, but passes with the PR.
    if (rank(x) /= 1) error stop
  end subroutine take_pointer_rank
end program main
