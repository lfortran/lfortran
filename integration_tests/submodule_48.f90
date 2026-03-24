module submodule_48_mod
  use, intrinsic :: iso_fortran_env, only: real32
  implicit none
  interface softmax
    module procedure softmax_r1_sp, softmax_r2_sp, softmax_r3_sp
  end interface
  interface
    pure module function softmax_r1_sp(x) result(y)
      real(real32), intent(in) :: x(:)
      real(real32) :: y(size(x))
    end function
    pure module function softmax_r2_sp(x, dim) result(y)
      real(real32), intent(in) :: x(:,:)
      integer, intent(in), optional :: dim
      real(real32) :: y(size(x,1), size(x,2))
    end function
    pure module function softmax_r3_sp(x, dim) result(y)
      real(real32), intent(in) :: x(:,:,:)
      integer, intent(in), optional :: dim
      real(real32) :: y(size(x,1), size(x,2), size(x,3))
    end function
  end interface
end module submodule_48_mod

submodule (submodule_48_mod) submodule_48_smod
contains
  pure module function softmax_r1_sp(x) result(y)
    real(real32), intent(in) :: x(:)
    real(real32) :: y(size(x))
    y = exp(x - maxval(x))
    y = y / sum(y)
  end function

  pure module function softmax_r2_sp(x, dim) result(y)
    real(real32), intent(in) :: x(:,:)
    integer, intent(in), optional :: dim
    real(real32) :: y(size(x,1), size(x,2))
    integer :: dim_, j

    dim_ = 1
    if (present(dim)) dim_ = dim

    if (dim_ < 2) then
      do j = 1, size(x, dim=2)
        y(:, j) = softmax(x(:, j))
      end do
    else
      do j = 1, size(x, dim=1)
        y(j, :) = softmax(x(j, :))
      end do
    end if
  end function

  pure module function softmax_r3_sp(x, dim) result(y)
    real(real32), intent(in) :: x(:,:,:)
    integer, intent(in), optional :: dim
    real(real32) :: y(size(x,1), size(x,2), size(x,3))
    integer :: dim_, j

    dim_ = 1
    if (present(dim)) dim_ = dim

    if (dim_ < 3) then
      do j = 1, size(x, dim=3)
        y(:,:,j) = softmax(x(:,:,j), dim=dim_)
      end do
    else
      do j = 1, size(x, dim=1)
        y(j,:,:) = softmax(x(j,:,:), dim=2)
      end do
    end if
  end function
end submodule submodule_48_smod

program submodule_48
  use, intrinsic :: iso_fortran_env, only: real32
  use submodule_48_mod, only: softmax
  implicit none

  real(real32), parameter :: tol = 1.0e-6_real32
  real(real32) :: x(2,2,2), y(2,2,2), y_ref(2,2,2), t(2)
  integer :: i, k

  x = reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32, &
               5.0_real32, 6.0_real32, 7.0_real32, 8.0_real32], [2,2,2])
  y = softmax(x, dim=2)

  do k = 1, 2
    do i = 1, 2
      t = exp(x(i,:,k) - maxval(x(i,:,k)))
      y_ref(i,:,k) = t / sum(t)
    end do
  end do

  print *, norm2(y-y_ref)
  if (norm2(y-y_ref) > tol) error stop 1

end program submodule_48
