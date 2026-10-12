module gpu_metal_430_g
  ! A generic procedure named like a derived type, `tensor_t(...)`, called
  ! in device code with a section that shared lowering hands over through a
  ! temporary. The temporary is named after the call, whose symbol is an
  ! alias that is not an identifier (`~tensor_t@construct`); the name of the
  ! temporary has to be one for the device source to compile.
  implicit none
  type :: tensor_t
    real :: s
  end type
  interface tensor_t
    module procedure construct
  end interface
contains
  pure function construct(v) result(t)
    real, intent(in) :: v(:)
    type(tensor_t) :: t
    t%s = sum(v)
  end function
end module

module gpu_metal_430_h
  use gpu_metal_430_g, only: tensor_t
  implicit none
contains
  pure function h(a) result(r)
    real, intent(in) :: a(:,:)
    real :: r
    real :: w(3, 2)
    w = a
    associate (n => 2)
      associate (t => tensor_t(w(1:n, 2)))
        r = 1 + t%s
      end associate
    end associate
  end function
end module

program gpu_metal_430
  use gpu_metal_430_h
  implicit none
  real :: x(3, 2, 2), s(2)
  integer :: k
  x = reshape([(real(k), k = 1, 12)], [3, 2, 2])
  do concurrent (k = 1:2)
    s(k) = 2*h(x(:, :, k))
  end do
  print *, s
  if (any(s /= [20., 44.])) error stop
end program
