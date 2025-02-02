module function_32_mod
contains
function matprod(x, y) result(z)
real(8), intent(in) :: x(:, :)
real(8), intent(in) :: y(:, :)
real(8) :: z(size(x, 1), size(y, 2))
integer :: i, j
z = 1.0_8
end function matprod
end module

program function_32
    real(8) :: x(5,2)
    x = 0
    call func(x)
contains 
   subroutine func(x)
   use function_32_mod
   real(8), intent(in) :: x(:,:)
   real(8) :: bmat(2, 2 + size(x,1))
   real(8) :: yzmat_c(2, size(x,2))
   integer :: npt 
   npt = size(x,1)
   bmat(:, 1:npt) = matprod(yzmat_c, transpose(x))
   if(any(bmat(:,1:npt) /= 1.0_8)) error stop
   end subroutine
end program 