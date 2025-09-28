program lapack_name_data
  implicit none
  integer :: n, incx
  real :: thresh
  real :: sx(8), stemp(8)
  real :: dv(8,5,2)
  real :: dtrue1(5)
  integer :: i

  dtrue1 = [ (/ 0.1, 0.2, 0.3, 0.4, 0.5 /) ]
  do i = 1, 8
     sx(i) = i
  end do
  do n = 0, 4
     do incx = 1, 2
        call sb1nrm2(n, (incx-2)*2, thresh)
        call sb1nrm2(n, incx, thresh)
        stemp(1) = dtrue1(n+1)
        call sb1nrm2(n, incx, thresh)
     end do
  end do
contains
  subroutine sb1nrm2(n, incx, thresh)
    integer, intent(in) :: n, incx
    real, intent(inout) :: thresh
    real :: work(10)
    work = 0
    thresh = incx + n
  end subroutine sb1nrm2
end program lapack_name_data
