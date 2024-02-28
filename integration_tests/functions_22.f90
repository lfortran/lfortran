pure integer function fn1(A) result(r)
implicit none
real, intent(in) :: A(:,:)
r = minval(shape(A))
end function

subroutine diag_rsp_mat(A, res)
implicit none
interface
    pure integer function fn1(B)
    implicit none
    real, intent(in) :: B(:,:)
    end function
end interface
real, intent(in) :: A(:,:)
real, intent(out) :: res(fn1(A))
print *, shape(res), fn1(A)
if (fn1(A) /= 5) error stop
end subroutine diag_rsp_mat

program functions_22
implicit none
real :: A(5, 5)
real :: res(5)
integer :: result
interface
    subroutine diag_rsp_mat(A, res)
    implicit none
    interface
        pure integer function fn1(C)
        implicit none
        real, intent(in) :: C(:,:)
        end function
    end interface
    real, intent(in) :: A(:,:)
    real, intent(out) :: res(fn1(A))
    end subroutine diag_rsp_mat
end interface
call diag_rsp_mat(A, res)
end program
