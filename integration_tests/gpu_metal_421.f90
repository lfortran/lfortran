module gpu_metal_421_m
  ! An associate name bound to a variable is that variable, in the loop
  ! body and in a routine the loop calls: what is written through the name
  ! reaches the selector. A selector designates the object it designates
  ! on entry, also when a subscript of it changes inside the construct.
  implicit none
  type :: pair_t
    real :: x, y
  end type
contains
  pure real function f(n, k, x, v) result(r)
    integer, value :: n
    integer, intent(in) :: k
    real, intent(in) :: x, v(3)
    integer :: m, j
    real :: w(3)
    type(pair_t) :: p
    m = 1
    associate (q => m)
      q = q + n
      associate (s => q)
        s = 2*s
      end associate
    end associate
    associate (d => n)
      d = d + k
    end associate
    w = 0
    j = 2
    associate (e => w(j), a => v(j))
      j = 3
      e = e + a + x
    end associate
    p%x = x
    p%y = 1
    associate (c => p%y, t => p)
      c = c + t%x
      t%x = 0
    end associate
    r = m + 10*n + 100*w(2) + 1000*w(3) + p%x + p%y
  end function
end module

program gpu_metal_421
  use gpu_metal_421_m
  implicit none
  integer, parameter :: n = 16
  real :: x(n), y(n), z(3, n), v(3)
  type(pair_t) :: u(n)
  integer :: i
  v = [1., 2., 3.]
  do i = 1, n
    x(i) = i
    z(:, i) = 0
    u(i)%x = i
    u(i)%y = 1
  end do
  do concurrent (i = 1:n)
    block
      integer :: m
      y(i) = f(i, 3, x(i), v) + 0
      m = 2
      associate (e => z(m, i), c => u(i)%y)
        m = 3
        associate (b => e)
          b = b + c
        end associate
        c = c + u(i)%x
      end associate
    end block
  end do
  do i = 1, n
    if (abs(y(i) - (2*(1 + i) + 10*(i + 3) + 100*(2 + x(i)) &
        + 1 + x(i))) > 1e-2) error stop 1
    if (z(2, i) /= 1 .or. z(1, i) /= 0 .or. z(3, i) /= 0) error stop 2
    if (abs(u(i)%y - (1 + i)) > 1e-5) error stop 3
  end do
  print *, "ok"
end program
