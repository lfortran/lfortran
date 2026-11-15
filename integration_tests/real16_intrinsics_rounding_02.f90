program real16_intrinsics_rounding_02
! sin, cos and tan of real(16) arguments are correctly rounded when folded
! (as GFortran folds them with MPFR) and within 1 ulp at run time (#17825).
! The references are the correctly rounded binary128 values, given as their
! (low, high) 64-bit words.
implicit none
integer, parameter :: qp = 16, n = 9
real(qp), parameter :: c(n) = [cos(1.0_qp), tan(1.0_qp), tan(2.5_qp), &
    tan(12.5_qp), tan(-1.5_qp), sin(0.5_qp), sin(2.5_qp), cos(0.5_qp), &
    cos(3.25_qp)]
integer(8), parameter :: w(2*n) = [ &
    -5106091812000444189_8, 4611145756655177832_8, &
    6596309442574299457_8, 4611561439776992826_8, &
    5389036925388217323_8, -4612109907190084687_8, &
    -6044978946535554725_8, -4613075521938364427_8, &
    8477417253917968528_8, -4610908393844275081_8, &
    -106005306692085704_8, 4611099903689797726_8, &
    579546200881654230_8, 4611178503362903211_8, &
    -442624094282320394_8, 4611335628559595476_8, &
    8863646168113145825_8, -4611970798102675585_8]
character(len=40) :: s
real(qp) :: x(n), r(n), ref
integer :: i, nfail

s = '1 1 2.5 12.5 -1.5 0.5 2.5 0.5 3.25'
read(s, *) x
r = [cos(x(1)), tan(x(2)), tan(x(3)), tan(x(4)), tan(x(5)), sin(x(6)), &
    sin(x(7)), cos(x(8)), cos(x(9))]
nfail = 0
do i = 1, n
    ref = transfer([w(2*i - 1), w(2*i)], 1.0_qp)
    call check(c(i), ref, 0, i, 'folded')
    call check(r(i), ref, 1, i, 'run time')
end do
if (nfail > 0) error stop
print *, "ok"

contains

    subroutine check(v, ref, tol, i, name)
        real(qp), intent(in) :: v, ref
        integer, intent(in) :: tol, i
        character(len=*), intent(in) :: name
        if (abs(v - ref) > tol*spacing(ref)) then
            print *, name, i, (v - ref)/spacing(ref), " ulp"
            nfail = nfail + 1
        end if
    end subroutine
end program
