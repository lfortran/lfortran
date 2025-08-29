submodule (linalg_separate_compilation_25) linalg_diag_separate_compilation_25
  implicit none

contains

  module function diag_real(A) result(res)
    real, intent(in) :: A(:,:)
    real :: res(minval(shape(A)))
    integer :: i
    do i = 1, minval(shape(A))
      res(i) = A(i,i)
    end do
  end function diag_real
end submodule