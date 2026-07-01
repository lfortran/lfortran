module operator_overloading_38_mod
  implicit none
  type :: scalar_t
    integer :: val = 0
  contains
    procedure :: mul_d
    generic :: operator(*) => mul_d
  end type
  type :: divergence_t
    integer :: tag = 7
  contains
    procedure, pass(d) :: premul
    generic :: operator(*) => premul
  end type
contains
  function mul_d(self, x) result(r)
    class(scalar_t), intent(in) :: self
    double precision, intent(in) :: x
    type(scalar_t) :: r
    r%val = self%val + int(x)
  end function
  function premul(s, d) result(r)
    type(scalar_t), intent(in) :: s
    class(divergence_t), intent(in) :: d
    type(scalar_t) :: r
    r%val = s%val + d%tag
  end function
end module

program operator_overloading_38
  use operator_overloading_38_mod
  type(scalar_t) :: f, r
  type(divergence_t) :: dv
  f%val = 3
  dv%tag = 7
  r = f * dv
  if (r%val /= 10) error stop
  r = f * 2.0d0
  if (r%val /= 5) error stop
  print *, "ok"
end program
