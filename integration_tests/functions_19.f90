pure function diag_rsp_mat(A) result(res)
real, intent(in) :: A(:,:)
real :: res(minval(shape(A)))

! res = 123.71_4
end function diag_rsp_mat

! program funtions_19
! real :: A(3,3)
! real :: res(3)
! interface
! ! pure function diag_rsp_mat(A) result(res)
! ! real, intent(in) :: A(:,:)
! ! real :: res(minval(shape(A)))
! ! end function diag_rsp_mat
! end interface

! ! res = diag_rsp_mat(A)
! print *, res
! ! if (any(abs(res - 123.71_4) > 1e-8)) error stop
! end program
