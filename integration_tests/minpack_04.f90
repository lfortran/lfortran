program minpack_lmder1_repro
  implicit none

  integer, parameter :: n = 3
  integer, parameter :: m = 15
  integer :: iflag, ldfjac
  real(8) :: x(n), xp(n), fvec(m), fvecp(m), fjac(m,n)

  x = (/ 1.0d0, 1.0d0, 1.0d0 /)
  ldfjac = m

  call check_deriv()

contains

  subroutine check_deriv()
    integer :: iflag
    xp = x
    iflag = 1
    call fcn(m, n, x, fvec, fjac, ldfjac, iflag)
    iflag = 2
    call fcn(m, n, x, fvec, fjac, ldfjac, iflag)
    iflag = 1
    call fcn(m, n, xp, fvecp, fjac, ldfjac, iflag)
  end subroutine check_deriv

  subroutine fcn(m, n, x, fvec, fjac, ldfjac, iflag)
    integer, intent(in) :: m
    integer, intent(in) :: n
    integer, intent(in) :: ldfjac
    real(8), intent(in) :: x(n)
    real(8), intent(inout) :: fvec(m)
    real(8), intent(inout) :: fjac(ldfjac, n)
    integer, intent(inout) :: iflag
    integer :: i
    real(8) :: tmp1, tmp2, tmp3, tmp4
    if (iflag == 1) then
      do i = 1, m
        tmp1 = dble(i)
        tmp2 = dble(m + 1 - i)
        if (i > m/2) then
          tmp3 = tmp2
        else
          tmp3 = tmp1
        end if
        fvec(i) = tmp1 + x(1) + tmp1/(x(2)*tmp2 + x(3)*tmp3)
      end do
    else
      do i = 1, m
        tmp1 = dble(i)
        tmp2 = dble(m + 1 - i)
        if (i > m/2) then
          tmp3 = tmp2
        else
          tmp3 = tmp1
        end if
        tmp4 = (x(2)*tmp2 + x(3)*tmp3)**2
        fjac(i,1) = -1.0d0
        fjac(i,2) = tmp1*tmp2/tmp4
        fjac(i,3) = tmp1*tmp3/tmp4
      end do
    end if
  end subroutine fcn

end program minpack_lmder1_repro

