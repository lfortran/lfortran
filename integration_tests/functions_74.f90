! A function whose explicit-shape result is sized by `size` of its
! assumed-shape dummy calls an internal function of the same form inside
! `matmul` (#2767).
function functions_74_attention(k) result(y)
    integer, parameter :: wp = kind(1.0)
    real(kind=wp), intent(in) :: k(:,:)
    real(kind=wp) :: y(size(k,1),size(k,2))
    y = matmul(k, softmax(k))

contains

    function softmax(x) result(y)
        integer, parameter :: wp = kind(1.0)
        real(kind=wp), intent(in) :: x(:,:)
        real(kind=wp) :: y(size(x,1),size(x,2))
        integer :: j
        do j = 1, size(x,2)
            y(:,j) = exp(x(:,j) - maxval(x(:,j)))
            y(:,j) = y(:,j) / sum(y(:,j))
        end do
    end function

end function

program functions_74
    implicit none
    interface
        function functions_74_attention(k) result(y)
            integer, parameter :: wp = kind(1.0)
            real(kind=wp), intent(in) :: k(:,:)
            real(kind=wp) :: y(size(k,1),size(k,2))
        end function
    end interface
    real :: k(2,2), y(2,2), s(2,2)
    integer :: i, j

    k = reshape([0.0, 0.0, 1.0, 2.0], [2, 2])
    y = functions_74_attention(k)
    print *, y

    ! Column 1 of k is constant, so softmax gives 0.5 in both rows.
    s(:,1) = 0.5
    s(1,2) = exp(1.0) / (exp(1.0) + exp(2.0))
    s(2,2) = exp(2.0) / (exp(1.0) + exp(2.0))
    do j = 1, 2
        do i = 1, 2
            if (abs(y(i,j) - sum(k(i,:) * s(:,j))) > 1e-6) error stop 1
        end do
    end do
    if (abs(y(1,1) - 0.5) > 1e-6) error stop 2
    if (abs(y(2,1) - 1.0) > 1e-6) error stop 3
end program
