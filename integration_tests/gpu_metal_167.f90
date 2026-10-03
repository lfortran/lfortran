module gpu_metal_167_activation_m
  implicit none
  type :: activation_t
    integer :: k = 1
  contains
    procedure :: f
  end type
contains
  elemental function f(self, x) result(r)
    class(activation_t), intent(in) :: self
    real, intent(in) :: x
    real :: r
    r = x * self%k
  end function
end module

module gpu_metal_167_m
  use gpu_metal_167_activation_m
  implicit none
  type :: t
    integer :: n = 3
    type(activation_t) :: act
  end type
contains
  pure function get_vals(a, i) result(v)
    real, intent(in) :: a(:,:)
    integer, intent(in) :: i
    real, allocatable :: v(:)
    allocate(v(size(a, 1)))
    v = a(:, i)
  end function

  subroutine run(self, a, nb, d)
    type(t), intent(in) :: self
    real, intent(in) :: a(:,:)
    integer, intent(in) :: nb
    real, intent(out) :: d(:)
    integer :: i
    real :: z(3)
    z = 1.0
    d = 0.0
    do concurrent (i = 1:nb)
      associate(y => get_vals(a, i))
        d(i) = y(1) * self%act%f(z(1))
      end associate
    end do
  end subroutine
end module

program gpu_metal_167
  use gpu_metal_167_m
  implicit none
  type(t) :: obj
  real :: a(3, 2), d(2)
  a = 0.5
  call run(obj, a, 2, d)
  print *, d(1)
  print *, d(2)
  if (abs(d(1) - 0.5) > 1e-6) error stop
  if (abs(d(2) - 0.5) > 1e-6) error stop
end program

