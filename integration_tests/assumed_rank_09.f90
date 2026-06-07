program assumed_rank_09
  implicit none
  integer, target :: a(2)

  call take_pointer_rank(a)
  print *, "ok"
contains
  subroutine take_pointer_rank(x)
    integer, pointer, intent(in) :: x(..)

    if (any(lbound(x) /= [1])) error stop
  end subroutine take_pointer_rank

end program assumed_rank_09