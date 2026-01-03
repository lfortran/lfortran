program test 
    implicit none
    real(8) :: y(2,2)
    real(8) :: x(2) = [1,2]
    real(8) :: z(size(y,2))
    integer :: j = 1
    z = matprod12(x, y)
    print *, z

contains
function matprod12(x, y) result(z)
    implicit none
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:, :)
    real(8) :: z(size(y, 2))
    integer :: j 
    j = size(y,2)
    z(j) = inprod(x, y(:, j))
end function matprod12

function inprod(x, y) result(z)
    implicit none
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    real(8) :: z 
end function inprod
end program

