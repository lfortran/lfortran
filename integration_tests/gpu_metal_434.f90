module gpu_metal_434_m
  ! A `real(8)` in a module procedure implemented in a submodule is on the
  ! Metal unsupported list like one in a module procedure: the loop that
  ! calls it runs on the CPU, in double precision.
  implicit none
  interface
    pure module function f(n) result(r)
      integer, intent(in) :: n
      integer :: r
    end function
  end interface
end module

submodule (gpu_metal_434_m) gpu_metal_434_s
  implicit none
contains
  module procedure f
    real(8) :: t
    t = n
    r = int(t)
  end procedure
end submodule

program gpu_metal_434
  use gpu_metal_434_m, only: f
  implicit none
  integer :: n(3), r(3), i
  n = [16777217, 16777219, 16777221]
  do concurrent (i = 1:3)
    r(i) = f(n(i))
  end do
  print *, r
  if (any(r /= n)) error stop
end program
