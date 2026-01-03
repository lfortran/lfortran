subroutine sub(simi, d)

    real, intent(inout) :: simi(:, :)
    real, intent(in) :: d(:)
    real :: simi_jdrop(size(simi, 2))

    simi_jdrop = 11.0
    simi_jdrop = matprod(simi, d)
    print *, "simi_jdrop = ", simi_jdrop
    if( any( abs(simi_jdrop - 468.00) > 1e-8 ) ) error stop
    if( size(simi_jdrop) /= 3 ) error stop
    simi = outprod(matprod(simi, d), simi_jdrop)

contains

    function outprod(x, y) result(z)
        implicit none
        real, intent(in) :: x(:)
        real, intent(in) :: y(:)
        real :: z(size(x), size(y))
        integer :: j

        z = 0.0
        do j = 1, size(y)
            z = z + x(j) * y(j)
        end do

    end function outprod

    function matprod(x, y) result(z_)
        implicit none
        real, intent(in) :: x(:, :)
        real, intent(in) :: y(:)
        real, allocatable :: z_(:)
        integer :: i, j

        allocate(z_(size(x, 2)))
        do i = 1, size(x, 2)
            z_(i) = 0.0
            do j = 1, size(y)
                z_(i) = z_(i) + x(i, j) * y(j)
            end do
        end do

    end function matprod

end subroutine

program arrays_82

real :: simi(3, 3)
real :: d(3)
interface

subroutine sub(simi, d)
    real, intent(inout) :: simi(:, :)
    real, intent(in) :: d(:)
end subroutine sub

end interface

simi = 12.0
d = 13.0
call sub(simi, d)
print *, simi
if( any(abs(simi - 657072) > 1e-8) ) error stop
if( size(simi) /= 9 ) error stop

end program
