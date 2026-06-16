program main
  implicit none
  integer, target :: a(2)

  call take_pointer_rank(a)
  print *, "ok"
contains
  subroutine take_pointer_rank(x)
    integer, pointer, intent(in) :: x(..)

    if (rank(x) == 1) then
        if (lbound(x, 1) /= 1) error stop
    else
        error stop
    end if
  end subroutine take_pointer_rank
end program main
