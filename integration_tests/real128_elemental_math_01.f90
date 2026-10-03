program real128_elemental_math_01
    ! elemental math intrinsics on real(16) variables (runtime path)
    implicit none
    real(16) :: x, y, one, two, six, nine
    integer :: n

    x = 0.5_16
    y = 0.25_16
    one = 1.0_16
    two = 2.0_16
    six = 6.0_16
    nine = 9.0_16
    n = 3

    call check(sqrt(x),     0.707106781186547524400844362104848992_16, 1)
    call check(exp(x),      1.64872127070012814684865078781416358_16, 2)
    ! negative literals are written as 0 - v until negative real(16) constants fold correctly
    call check(log(x),     0.0_16 - 0.693147180559945309417232121458176575_16, 3)
    call check(log10(x),   0.0_16 - 0.301029995663981195213738894724493020_16, 4)
    call check(sin(x),      0.479425538604203000273287935215571402_16, 5)
    call check(cos(x),      0.877582561890372716116281582603829681_16, 6)
    call check(tan(x),      0.546302489843790513255179465780285354_16, 7)
    call check(asin(x),     0.523598775598298873077107230546583832_16, 8)
    call check(acos(x),     1.04719755119659774615421446109316766_16, 9)
    call check(atan(x),     0.463647609000806116214256231461214397_16, 10)
    call check(sinh(x),     0.521095305493747361622425626411491546_16, 11)
    call check(cosh(x),     1.12762596520638078522622516140267203_16, 12)
    call check(tanh(x),     0.462117157260009758502318483643672557_16, 13)
    call check(asinh(x),    0.481211825059603447497758913424368471_16, 14)
    call check(acosh(x+1),  0.962423650119206894995517826848736845_16, 15)
    call check(atanh(x),    0.549306144334054845697622618461262902_16, 16)
    call check(atan2(x,y),  1.10714871779409050301706546017853705_16, 17)
    call check(x**y,        0.840896415253714543031125476233214847_16, 18)
    call check(hypot(x,y),  0.559016994374947424102293417182819080_16, 19)
    call check(atan(one), 0.785398163397448309615660845819875721_16, 20)
    call check(sqrt(nine), 3.0_16, 21)
    call check(six/two, 3.0_16, 22)
    call check(x - 0.75_16, 0.0_16 - 0.25_16, 23)
    call check(exp(-x*40),  2.06115362243855782796594038015582110E-0009_16, 24)
    call check(sin(x*200), 0.0_16 - 0.506365641109758793656557610459785419_16, 25)
    call check(tanh(20.0_16), 0.999999999999999991503291489416822065_16, 26)
    call check(x**n,        0.125_16, 27)
    print *, "ok"
contains
    subroutine check(got, expected, id)
        real(16), intent(in) :: got, expected
        integer, intent(in) :: id
        real(16), parameter :: tol = 1.0e-31_16
        if (abs(got - expected) > tol * max(abs(expected), 1.0_16)) then
            print *, id, got, expected
            error stop
        end if
    end subroutine
end program
