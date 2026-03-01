module custom_operator_18_mod
  implicit none
  interface operator(.pinv.)
     module procedure pinv_18
  end interface
contains
  function pinv_18(a) result(r)
     real, intent(in) :: a(:,:)
     real :: r(size(a,2),size(a,1))
     integer :: i, j
     do i = 1, size(a,2)
       do j = 1, size(a,1)
         r(i,j) = a(j,i)
       end do
     end do
  end function
end module

program custom_operator_18
  use custom_operator_18_mod, only: operator(.pinv.)
  implicit none
  real :: A(3,2), B(2,3)
  integer :: i, j, k

  k = 1
  do i = 1, 3
    do j = 1, 2
      A(i,j) = real(k)
      k = k + 1
    end do
  end do

  B = .pinv.A

  ! A = [[1,2],[3,4],[5,6]]
  ! B (transpose) = [[1,3,5],[2,4,6]]
  if (abs(B(1,1) - 1.0) > 1e-6) error stop
  if (abs(B(1,2) - 3.0) > 1e-6) error stop
  if (abs(B(1,3) - 5.0) > 1e-6) error stop
  if (abs(B(2,1) - 2.0) > 1e-6) error stop
  if (abs(B(2,2) - 4.0) > 1e-6) error stop
  if (abs(B(2,3) - 6.0) > 1e-6) error stop
  print *, "PASSED"
end program
