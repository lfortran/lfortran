program arrays_111
  implicit none
  integer, parameter :: k = 2000
  integer :: i, nsize
  real :: y(k) = 1.0

  do i = 1000, k, 500
    nsize = size(outprod(y(1:i), y(1:i)))
    if (nsize /= i * i) error stop
    print "(2(1X,I0))", i, nsize
  end do

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
end program arrays_111
