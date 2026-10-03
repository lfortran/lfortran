! Test that a module with an abstract interface used as procedure pointer
! type in a module function is deserialized correctly when the submodule
! is loaded during separate compilation.
program submodule_27
  use submodule_27_m, only: vec_t, fn_i, make_vec
  implicit none
  type(vec_t) :: v
  procedure(fn_i), pointer :: f
  f => double_it
  v = make_vec(f)
  if (abs(v%val - 6.0d0) > 1.0d-10) error stop
  print *, "ok"
contains
  pure function double_it(x) result(y)
    real(8), intent(in) :: x
    real(8) :: y
    y = 2.0d0 * x
  end function
end program submodule_27
