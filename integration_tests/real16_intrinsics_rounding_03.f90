program real16_intrinsics_rounding_03
! erf and erfc of real(16) arguments are correctly rounded when folded (as
! GFortran folds them with MPFR) and within 1 ulp at run time (#17825),
! including erf for 7 < x < 8.75, where erf(x) is not yet 1, and erfc of
! large arguments, and erf of tiny arguments (2^-60 < x < 2^-57), where the
! x^3 term still matters. The references are the correctly rounded
! binary128 values, given as their (low, high) 64-bit words.
implicit none
integer, parameter :: qp = 16, n = 13
real(qp), parameter :: c(n) = [erf(1.0_qp), erf(0.75_qp), erf(1.75_qp), &
    erf(7.5_qp), erfc(1.5_qp), erfc(1.75_qp), erfc(4.5_qp), &
    erfc(10.5_qp), erfc(-1.5_qp), erfc(30.0_qp), erfc(80.5_qp), &
    erf(5.97e-18_qp), erf(2.76e-18_qp)]
integer(8), parameter :: w(2*n) = [ &
    -6022155954236486361_8, 4611315991869395080_8, &
    -665111365371751090_8, 4611241938528096485_8, &
    -3236210309790196515_8, 4611397040268610902_8, &
    -288343758_8, 4611404543450677247_8, &
    5627125790801244967_8, 4610020991289820448_8, &
    4203275015767509153_8, 4609632947289238091_8, &
    5295732102290689694_8, 4602309782667172163_8, &
    2313020074657707338_8, 4565424278362913864_8, &
    -175847680962538905_8, 4611676477874281430_8, &
    -8607796033499533003_8, 4244302435995882658_8, &
    -4186772981866572704_8, 1977858176289501821_8, &
    -1612404613848231111_8, 4595344043437860116_8, &
    -7052698961661545837_8, 4595021373213929995_8]
character(len=80) :: s
real(qp) :: x(n), r(n), ref
integer :: i, nfail

s = '1 0.75 1.75 7.5 1.5 1.75 4.5 10.5 -1.5 30 80.5 5.97e-18 2.76e-18'
read(s, *) x
r = [erf(x(1)), erf(x(2)), erf(x(3)), erf(x(4)), erfc(x(5)), erfc(x(6)), &
    erfc(x(7)), erfc(x(8)), erfc(x(9)), erfc(x(10)), erfc(x(11)), &
    erf(x(12)), erf(x(13))]
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
