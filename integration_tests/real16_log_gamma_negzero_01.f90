program real16_log_gamma_negzero_01
! real(16) log_gamma keeps its relative accuracy next to the zeros of
! log|gamma(x)| on the negative axis (#17858), both folded and at run time.
! x(4:10) are the binary128 values nearest to seven of these zeros, where
! the result is below 1e-26 except near -29. The references are the correctly rounded binary128
! values, given as their (low, high) 64-bit words, like the arguments.
implicit none
integer, parameter :: qp = 16, n = 10
real(qp), parameter :: xc(n) = [-2.4570247382208_qp, -2.7476826467274_qp, &
    -2.45702_qp, -2.457024738220800623039454147651179543237_qp, &
    -2.747682646727412601391488482691499695862_qp, &
    -3.143580888349980058694358781820227899567_qp, &
    -5.008218168322593521552368137391369709564_qp, &
    -7.999975197095820664154336147166807_qp, &
    -10.00000027557301364660024986741209776248_qp, &
    -29.00000000000000000000000000000011309963_qp]
real(qp), parameter :: lc(n) = log_gamma(xc)
integer(8), parameter :: xw(2*n) = [ &
    -6132935129901614697_8, -4611621697913614458_8, &
    5096236856892917578_8, -4611580791449600626_8, &
    47223664828696452_8, -4611621698580459752_8, &
    -4515432229644563196_8, -4611621697913614458_8, &
    917831428881911639_8, -4611580791449600624_8, &
    -586489171075797795_8, -4611525073725430374_8, &
    -8013430865438355885_8, -4611333596404315283_8, &
    -1084620251699041230_8, -4611123070219315812_8, &
    8297027493382859400_8, -4611052699720093065_8, &
    37_8, -4610612895078678528_8]
integer(8), parameter :: lw(2*n) = [ &
    669117014934904872_8, 4597348574151716827_8, &
    -3816851568116768356_8, -4624719002637592397_8, &
    7186466014885564678_8, 4606586410681860804_8, &
    -5184560483867738511_8, -4643378827633315576_8, &
    4353815567703704218_8, -4643371522689253474_8, &
    -3268350656531054647_8, 4580312595928364543_8, &
    -5605888273905625474_8, 4582090377503645959_8, &
    -7977798745729971709_8, -4638817941289466930_8, &
    -7524928599838133727_8, 4586247110192998807_8, &
    -9146035326813123860_8, -4613928851417917091_8]
real(qp) :: x, l_ref
integer :: i, nfail

nfail = 0
do i = 1, n
    x = q(xw(2*i - 1), xw(2*i))
    l_ref = q(lw(2*i - 1), lw(2*i))
    if (x /= xc(i)) then
        print *, "argument", i, " differs from the literal"
        nfail = nfail + 1
    end if
    call check(lc(i), l_ref, i, 'folded log_gamma')
    call check(log_gamma(x), l_ref, i, 'log_gamma')
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
