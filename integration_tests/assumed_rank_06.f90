module assumed_rank_06_mod
  implicit none
  type :: t
    real, allocatable :: x(:)
  end type
contains
  subroutine sub(x)
    real, intent(in) :: x(..)
    select rank(x)
      rank(1)
        if (size(x) /= 3) error stop
        if (abs(x(1) - 1.0) > 1e-6) error stop
        if (abs(x(2) - 2.0) > 1e-6) error stop
        if (abs(x(3) - 3.0) > 1e-6) error stop
      rank default
        error stop
    end select
  end subroutine
end module

program assumed_rank_06
  use assumed_rank_06_mod
  implicit none
  type(t) :: a
  allocate(a%x(3))
  a%x(1) = 1.0
  a%x(2) = 2.0
  a%x(3) = 3.0
  call sub(a%x)
  print *, "ok"
end program
