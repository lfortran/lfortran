! Test: submodule struct constructor returning a derived type with array argument,
! called from do concurrent. Exercises two fixes:
! 1. types_equal() must compare GPU-duplicated StructTypes structurally,
!    not by pointer identity.
! 2. pass_array_by_data must clean up stale ExternalSymbols in GPU kernel scopes.
module gpu_metal_152_m
  implicit none

  type :: result_t
    real :: val
  end type

  interface result_t
    pure module function construct(v) result(r)
      real, intent(in) :: v(:)
      type(result_t) :: r
    end function
  end interface

  interface
    elemental module function do_work(self, x) result(out)
      type(result_t), intent(in) :: self
      real, intent(in) :: x
      type(result_t) :: out
    end function
  end interface

end module

submodule(gpu_metal_152_m) gpu_metal_152_impl
  implicit none
contains

  pure module function construct(v) result(r)
    real, intent(in) :: v(:)
    type(result_t) :: r
    integer :: i
    r%val = 0.0
    do i = 1, size(v)
      r%val = r%val + v(i)
    end do
  end function

  module procedure do_work
    out%val = self%val + x
  end procedure

end submodule

program gpu_metal_152
  use gpu_metal_152_m, only: result_t, do_work
  implicit none

  integer, parameter :: n = 4
  type(result_t) :: inputs(n), outputs(n)
  real :: vals(n)
  integer :: i

  vals = [1.0, 2.0, 3.0, 4.0]
  do i = 1, n
    inputs(i)%val = real(i)
  end do

  do concurrent(i = 1:n)
    outputs(i) = do_work(inputs(i), vals(i))
  end do

  ! do_work(inputs(i), vals(i)) = inputs(i)%val + vals(i) = i + i = 2*i
  if (abs(outputs(1)%val - 2.0) > 1e-5) error stop
  if (abs(outputs(2)%val - 4.0) > 1e-5) error stop
  if (abs(outputs(3)%val - 6.0) > 1e-5) error stop
  if (abs(outputs(4)%val - 8.0) > 1e-5) error stop

  print *, "ok"
end program
