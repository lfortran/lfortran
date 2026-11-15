program real16_gamma_02
! gamma and log_gamma of real(16) arguments are within 2 ulp of the
! correctly rounded value, both folded and at run time (#17825). This
! includes log_gamma near its zeros 1 and 2 and the reflection formula for
! negative arguments. The references are the correctly rounded binary128
! values, given as their (low, high) 64-bit words.
implicit none
integer, parameter :: qp = 16, n = 10
real(qp), parameter :: e40 = 2.0_qp**(-40)
real(qp), parameter :: gc(n) = [gamma(2.5_qp), gamma(0.5_qp), &
    gamma(1.5_qp), gamma(0.25_qp), gamma(7.25_qp), gamma(100.5_qp), &
    gamma(-2.5_qp), gamma(-0.5_qp), gamma(1 + e40), gamma(2 - e40)]
real(qp), parameter :: lc(n) = [log_gamma(2.5_qp), log_gamma(0.5_qp), &
    log_gamma(1.5_qp), log_gamma(0.25_qp), log_gamma(7.25_qp), &
    log_gamma(100.5_qp), log_gamma(-2.5_qp), log_gamma(-0.5_qp), &
    log_gamma(1 + e40), log_gamma(2 - e40)]
integer(8), parameter :: gw(2*n) = [ &
    -10323571338770869_8, 4611497244528769848_8, &
    -6162679453021545031_8, 4611621969880370934_8, &
    -6162679453021545031_8, 4611340494903660278_8, &
    -6643481997066329259_8, 4611914802682916590_8, &
    5421504437686907468_8, 4614255406955902315_8, &
    -5464322758827134263_8, 4758153715637926080_8, &
    3264738756088779496_8, -4611998281857355240_8, &
    -6162679453021545031_8, -4611468591997694218_8, &
    8588427189892301440_8, 4611404543450676952_8, &
    -8588427177858693632_8, 4611404543450677031_8]
integer(8), parameter :: lw(2*n) = [ &
    8788493693660757461_8, 4610880642937890430_8, &
    -3405460647848541121_8, 4611163806315215387_8, &
    1500157790036696373_8, -4612830913447244775_8, &
    -5628629737389096512_8, 4611485614584108783_8, &
    6807842732395546330_8, 4612182271861264368_8, &
    9097074572442818976_8, 4613772270867087664_8, &
    6381252591421477765_8, -4613149744903040783_8, &
    7064592755490375187_8, 4611479278469451485_8, &
    8422714596467834013_8, -4623464498894276164_8, &
    -6007680244698343300_8, -4623594904559155214_8]
character(len=40) :: s
real(qp) :: x(n), g_ref, l_ref
integer :: i, nfail

s = '2.5 0.5 1.5 0.25 7.25 100.5 -2.5 -0.5'
read(s, *) x(1:8)
x(9) = (x(3) - x(2)) + e40
x(10) = 2*(x(3) - x(2)) - e40
nfail = 0
do i = 1, n
    g_ref = q(gw(2*i - 1), gw(2*i))
    l_ref = q(lw(2*i - 1), lw(2*i))
    call check(gc(i), g_ref, i, 'folded gamma')
    call check(lc(i), l_ref, i, 'folded log_gamma')
    call check(gamma(x(i)), g_ref, i, 'gamma')
    call check(log_gamma(x(i)), l_ref, i, 'log_gamma')
end do
if (nfail > 0) error stop
print *, "ok"

contains

    real(qp) function q(lo, hi)
        integer(8), intent(in) :: lo, hi
        q = transfer([lo, hi], 1.0_qp)
    end function

    subroutine check(v, ref, i, name)
        real(qp), intent(in) :: v, ref
        integer, intent(in) :: i
        character(len=*), intent(in) :: name
        if (abs(v - ref) > 2*spacing(ref)) then
            print *, name, i, (v - ref)/spacing(ref), " ulp"
            nfail = nfail + 1
        end if
    end subroutine
end program
