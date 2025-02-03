module function_32_mod
contains
function matprod(x, y) result(z)
    real(8), intent(in) :: x(:, :)
    real(8), intent(in) :: y(:, :)
    real(8) :: z(size(x, 1), size(y, 2))
    integer :: i, j
    z = 1.0_8
end function matprod
function return_type_check(x) result(z)
    real(8), intent(in) :: x(:, :)
    real(8) :: z(size(x))
    z = 1.0_8
end function 
end module

program function_32
    use function_32_mod
    real(8) :: x(5,2)
    integer :: iact(2)
    real(8) :: full_size(10)
    iact = [2,1]
    full_size = return_type_check(x(:,iact)) + 1.0_8
    if(any(full_size /= 2.0_8)) error stop
    x = matprod(x(:,iact), x(:, iact)) + 1.0_8
    if(any(x /= 2.0_8)) error stop
    call func(x, iact)
contains 
subroutine func(x, iact)
    real(8), intent(in) :: x(:,:)
    integer, intent(inout) :: iact(:)
    real(8) :: bmat(2, 2 + size(x,1))
    real(8) :: yzmat_c(2, size(x,2))
    real(8) :: tmp_x(size(x,1) * size(x,2))
    integer :: npt 
    npt = size(x,1)
    bmat(:, 1:npt) = matprod(yzmat_c, transpose(x))
    if(any(bmat(:,1:npt) /= 1.0_8)) error stop
    tmp_x = return_type_check(x(:,iact)) + 1.0_8
    if(any(tmp_x /= 2.0_8)) error stop
    yzmat_c = matprod(x(iact, :), x(:, iact)) + 1.0_8
    if(any(yzmat_c /= 2.0_8)) error stop
end subroutine
end program 