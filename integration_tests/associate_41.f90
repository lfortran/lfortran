module associate_statement_function_01_m
  implicit none
  integer, parameter :: r8 = kind(1.0d0)

contains

  real(r8) function student(dof, a) result(t)
    integer, intent(in) :: dof
    real(r8), intent(in) :: a

    real(r8) :: f, fold, x, p, integral
    real(r8), parameter :: dt = 1.0e-2_r8

    ! Regression: statement function called from ASSOCIATE expression.
    f(x) = (1.0_r8 + x**2 / real(dof, r8))**(-(dof + 1) / 2.0_r8)

    integral = 0.0_r8
    fold = 0.0_r8
    p = 0.5_r8 * a
    t = 1.0e3_r8

    do while (f(t) > 1.0e-20_r8)
      t = t * 2.0_r8
    end do

    do while (integral < p)
      associate (fnew => f(t + dt))
        integral = integral + 0.5_r8 * (fold + fnew) * dt
        t = t - dt
        fold = fnew
        if (t < 0.0_r8) stop 1
      end associate
    end do
  end function student

end module associate_statement_function_01_m

program associate_41
  use associate_statement_function_01_m, only: student, r8
  implicit none
  print *, student(10, 0.05_r8)
end program associate_41
