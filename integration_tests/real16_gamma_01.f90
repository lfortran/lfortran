program real16_gamma_01
implicit none
! gamma, log_gamma and erfc_scaled of non-constant real(16) arguments
! (#15736, #17463). Reference values are from GFortran/libquadmath and the
! accuracy checked is a relative error below 1e-30.
integer, parameter :: qp = 16
real(qp), parameter :: tol = 1.0e-30_qp
real(qp) :: x(8), g(8), l(8), e(8), y
real(qp), parameter :: g_ref(8) = [ &
    3.625609908221908311930685155867672087_qp, &
    1.772453850905516027298167483341145140_qp, &
    0.8862269254527580136490837416705725702_qp, &
    2.0_qp, &
    1155.381013919989687202703767970556556_qp, &
    5.406242982335075044736873647808221288e17_qp, &
    9.320963104082716608349109809141910822e156_qp, &
    -0.9453087204829418812256893244486106967_qp]
real(qp), parameter :: l_ref(8) = [ &
    1.288022524698077457370610440219717262_qp, &
    0.5723649429247000870717136756765293281_qp, &
    -0.1207822376352452223455184457816472108_qp, &
    0.6931471805599453094172321214581765751_qp, &
    7.052185450738539444925749253133010595_qp, &
    40.83150097453079810977608746076652049_qp, &
    361.4355404677776215552519127025208074_qp, &
    0.2846828704729191596324946696827019332_qp]
real(qp), parameter :: e_ref(8) = [ &
    0.7703465477309967439167391723367911514_qp, &
    0.6156903441929258748707934226837419242_qp, &
    0.3215854164543175023543225877232655695_qp, &
    0.1790011511813899504192948153136210051_qp, &
    0.07709918035125990166373285352310791834_qp, &
    0.0274888151519348721256473397546552745_qp, &
    0.005613548837895436229888757658162255712_qp, &
    1035.814842972622908298729932389364058_qp]
integer :: i

x = [0.25_qp, 0.5_qp, 1.5_qp, 3.0_qp, 7.25_qp, 20.5_qp, 100.5_qp, -2.5_qp]
do i = 1, 8
    g(i) = gamma(x(i))
    l(i) = log_gamma(abs(x(i)))
    e(i) = erfc_scaled(x(i))
end do
do i = 1, 8
    if (.not. close(g(i), g_ref(i))) error stop
    if (.not. close(l(i), l_ref(i))) error stop
    if (.not. close(e(i), e_ref(i))) error stop
end do

! reflection formula for negative non-integer arguments
y = -2.5_qp
y = log_gamma(y)
if (.not. close(y, -5.624371649767405067259453009765427975e-2_qp)) error stop
y = -0.5_qp
y = gamma(y)
if (.not. close(y, (-3.544907701811032054596334966682290281e0_qp))) error stop

! erfc_scaled stays finite where exp(x**2) overflows
y = 200.0_qp
y = erfc_scaled(y)
if (abs(y - 2.820912657212046398684609743699463594e-3_qp) > &
    tol*2.820912657212046398684609743699463594e-3_qp) error stop
print *, "ok"

contains

    logical function close(a, b)
        real(qp), intent(in) :: a, b
        close = abs(a - b) <= tol*abs(b)
    end function
end program
