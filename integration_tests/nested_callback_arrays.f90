module linalg_mod
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: inprod, matprod12

contains

  function inprod(x, y) result(z)
    real(real64), intent(in) :: x(:)
    real(real64), intent(in) :: y(:)
    real(real64) :: z
    integer :: i

    z = 0.0_real64
    do i = 1, int(size(x), kind(i))
      z = z + x(i) * y(i)
    end do
  end function inprod

  function matprod12(x, y) result(z)
    real(real64), intent(in) :: x(:)
    real(real64), intent(in) :: y(:, :)
    real(real64) :: z(size(y, 2))
    integer :: j

    do j = 1, int(size(y, 2), kind(j))
      z(j) = inprod(x, y(:, j))
    end do
  end function matprod12

end module linalg_mod

module caller_mod
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: call_calcfc

  abstract interface
    subroutine objcon(x, constr)
      import real64
      real(real64), intent(in) :: x(:)
      real(real64), intent(out) :: constr(:)
    end subroutine objcon
  end interface

contains

  subroutine call_calcfc(calcfc, x, constr)
    procedure(objcon) :: calcfc
    real(real64), intent(in) :: x(:)
    real(real64), intent(out) :: constr(:)

    call calcfc(x, constr)
  end subroutine call_calcfc

end module caller_mod

program nested_callback_arrays
  use iso_fortran_env, only: real64
  use linalg_mod, only: matprod12
  use caller_mod, only: call_calcfc
  implicit none

  real(real64) :: amat(2, 1)
  real(real64) :: bvec(1)
  real(real64) :: x(2)
  real(real64) :: constr(1)

  amat = 1.0_real64
  bvec = 0.0_real64
  x = [1.0_real64, 2.0_real64]

  call call_calcfc(calcfc_internal, x, constr)

  if (constr(1) /= 3.0_real64) error stop

contains

  subroutine calcfc_internal(x_internal, constr_internal)
    real(real64), intent(in) :: x_internal(:)
    real(real64), intent(out) :: constr_internal(:)

    constr_internal = matprod12(x_internal, amat) - bvec
  end subroutine calcfc_internal

end program nested_callback_arrays
