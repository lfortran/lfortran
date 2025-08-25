module linalg_separate_compilation_25
  implicit none

  interface diag
      module function diag_real(A) result(res)
        real, intent(in) :: A(:,:)
        real :: res(minval(shape(A)))
      end function diag_real
  end interface
end module linalg_separate_compilation_25


module stats_corr_separate_compilation_25
  use linalg_separate_compilation_25, only: diag
  implicit none

  interface corr
      procedure corr_real
  end interface corr

contains

  module function corr_real(x) result(res)
    real, intent(in) :: x(:, :)
    real :: res(2)
    res = diag(x)
  end function corr_real
end module