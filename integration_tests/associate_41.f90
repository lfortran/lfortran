module associate_41_m
  implicit none
contains
  subroutine sub()
    real :: f, x, result

    f(x) = x * 2.0

    associate (fnew => f(1.0))
      result = fnew
    end associate

    if (abs(result - 2.0) > 1e-6) error stop
  end subroutine sub
end module associate_41_m

program associate_41
  use associate_41_m, only: sub
  implicit none
  call sub()
end program associate_41
