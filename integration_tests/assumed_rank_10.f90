program main
  implicit none
  integer, target :: a(2)

  call take_pointer_rank(a)
  print *, "ok"
contains
  subroutine take_pointer_rank(x)
    integer, pointer, intent(in) :: x(..)

    select rank (x)
      rank (1)
        if (lbound(x, 1) /= 1) error stop
      rank default
        error stop
    end select
  end subroutine take_pointer_rank
end program main
