! Test function result with specification expression using size() (issue #4656)
program functions_54
    implicit none
    integer, parameter :: k = 100
    integer :: nsize
    real :: y(k) = 1.0

    nsize = size(outprod(y(1:50), y(1:50)))
    if (nsize /= 2500) error stop

contains
    function outprod(y, z) result(x)
        implicit none
        real, intent(in) :: y(:), z(:)
        real :: x(size(y), size(z))
        integer :: i, j

        do i = 1, size(y)
            do j = 1, size(z)
                x(i, j) = y(i) * z(j)
            end do
        end do
    end function outprod
end program functions_54
