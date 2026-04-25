module gpu_metal_104_m
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  end type
contains
  subroutine compute(inputs, n, results, &
      a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, &
      a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, &
      a21, a22, a23, a24, a25, a26, a27, a28, a29, a30)
    type(tensor_t), intent(in) :: inputs(:)
    integer, intent(in) :: n
    real, intent(out) :: results(n)
    real, intent(in) :: a1(:), a2(:), a3(:), a4(:), a5(:)
    real, intent(in) :: a6(:), a7(:), a8(:), a9(:), a10(:)
    real, intent(in) :: a11(:), a12(:), a13(:), a14(:), a15(:)
    real, intent(in) :: a16(:), a17(:), a18(:), a19(:), a20(:)
    real, intent(in) :: a21(:), a22(:), a23(:), a24(:), a25(:)
    real, intent(in) :: a26(:), a27(:), a28(:), a29(:), a30(:)
    integer :: i
    do concurrent (i = 1:n)
      results(i) = inputs(i)%values_(1) + &
        a1(i) + a2(i) + a3(i) + a4(i) + a5(i) + &
        a6(i) + a7(i) + a8(i) + a9(i) + a10(i) + &
        a11(i) + a12(i) + a13(i) + a14(i) + a15(i) + &
        a16(i) + a17(i) + a18(i) + a19(i) + a20(i) + &
        a21(i) + a22(i) + a23(i) + a24(i) + a25(i) + &
        a26(i) + a27(i) + a28(i) + a29(i) + a30(i)
    end do
  end subroutine
end module

program gpu_metal_104
  use gpu_metal_104_m
  implicit none
  integer, parameter :: n = 4
  type(tensor_t) :: inputs(n)
  real :: results(n), zeros(n)
  integer :: i
  zeros = 0.0
  do i = 1, n
    inputs(i) = tensor_t(values_=[real(i)])
  end do
  call compute(inputs, n, results, &
    zeros, zeros, zeros, zeros, zeros, zeros, zeros, zeros, zeros, zeros, &
    zeros, zeros, zeros, zeros, zeros, zeros, zeros, zeros, zeros, zeros, &
    zeros, zeros, zeros, zeros, zeros, zeros, zeros, zeros, zeros, zeros)
  do i = 1, n
    if (abs(results(i) - real(i)) > 1.0e-6) error stop
  end do
  print *, "ok"
end program
