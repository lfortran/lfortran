program main
  implicit none
  integer, target :: a(2)

  call take_pointer_rank(a)
  print *, "ok"
contains
  subroutine take_pointer_rank(x)
    integer, pointer, intent(in) :: x(..)
    call check_rank(x)
  end subroutine take_pointer_rank

  subroutine check_rank(y)
    integer, intent(in) :: y(..)

    if (rank(y) /= 1) error stop
  end subroutine check_rank
end program main
