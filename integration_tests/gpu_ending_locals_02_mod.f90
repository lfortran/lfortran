module gpu_ending_locals_02_mod
implicit none
private
public :: matvec
contains
! Its contiguous dummies get the extents pass_array_by_data adds, scalars that
! its launches are handed by value.
pure subroutine matvec(x, w, y)
    real, contiguous, intent(in) :: x(:), w(:,:)
    real, contiguous, intent(out) :: y(:)
    real :: xs(size(x))
    integer :: i, j
    do concurrent (j = 1:size(x))
        xs(j) = 2*x(j)
    end do
    do concurrent (i = 1:size(y))
        block
            real :: acc
            integer :: k
            acc = 0
            do k = 1, size(x)
                acc = acc + w(k,i)*xs(k)
            end do
            y(i) = acc
        end block
    end do
end subroutine
end module
