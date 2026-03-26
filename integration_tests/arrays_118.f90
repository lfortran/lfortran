module arrays_118_mod
  use, intrinsic :: iso_fortran_env, only: real32
  implicit none

  interface
    module function mean_2_rsp_rsp(x, dim) result(res)
      real(real32), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      real(real32) :: res(merge(size(x,1), size(x,2), dim==2))
    end function mean_2_rsp_rsp
  end interface

end module arrays_118_mod


submodule (arrays_118_mod) arrays_118_smod
  implicit none

contains

  module function mean_2_rsp_rsp(x, dim) result(res)
    real(real32), intent(in) :: x(:, :)
    integer, intent(in) :: dim
    real(real32) :: res(merge(size(x,1), size(x,2), dim==2))

    if (dim >= 1 .and. dim <= 2) then
      res = sum(x, dim) / real(size(x, dim), real32)
    else
      error stop "ERROR: wrong dimension"
    end if

  end function mean_2_rsp_rsp

end submodule arrays_118_smod


program arrays_118
  use arrays_118_mod, only: mean_2_rsp_rsp
  use, intrinsic :: iso_fortran_env, only: real32
  implicit none

  real(real32), parameter :: tol = 1e-5_real32

  ! 4x3 matrix
  real(real32), parameter :: x2(4,3) = reshape([ &
       1.0_real32, 3.0_real32, 5.0_real32, 7.0_real32, &
       2.0_real32, 4.0_real32, 6.0_real32, 8.0_real32, &
       9.0_real32,10.0_real32,11.0_real32,12.0_real32 ], [4,3])

  real(real32) :: expected(3)
  real(real32) :: result(3)

  expected = [21.0, 30.0, 111.5]

  ! Actual
  result = mean_2_rsp_rsp(x2**2, 1)

  ! Debug print (optional but useful for compiler issues)
  print *, "Expected:", expected
  print *, "Result  :", result

  ! Check
  if (.not. all(abs(expected - result) < tol)) then
     error stop "Test FAILED"
  end if

  print *, "Test PASSED"

end program arrays_118
