! LFortran traits extension test; GFortran does not accept this syntax.
! Kind wildcards in type sets (traits.tex, "Assumed kind parameters"):
! integer(*) | real(*) | complex(*) is the union of the integer, real and
! complex types of every kind that LFortran supports, i.e. ISO_FORTRAN_ENV's
! INTEGER_KINDS and REAL_KINDS and the real kinds with a complex type. Every
! member is called, statically and through the closed runtime slots of the
! separately compiled contracts and providers, with checked values and kinds;
! a generic over the same members written out gives the same results.
program traits_kind_sets_01
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, &
        real32, real64, real128, integer_kinds, real_kinds
    use traits_kind_sets_01_contracts, only: ISum, ICount, IMax
    use traits_kind_sets_01_providers, only: Summer, total, total_explicit
    implicit none
    integer(int8)   :: i1(4)
    integer(int16)  :: i2(3)
    integer(int32)  :: i4(3)
    integer(int64)  :: i8(2)
    real(real32)    :: r4(2)
    real(real64)    :: r8(2)
    real(real128)   :: r16(3), tiny128
    complex(real32) :: c4(3)
    complex(real64) :: c8(2)
    type(Summer)    :: provider

    ! The members are exactly these kinds: a kind added to either list must
    ! also be checked here.
    if (size(integer_kinds) /= 4 .or. any(integer_kinds /= [int8, int16, int32, int64])) error stop 1
    if (size(real_kinds) /= 3 .or. any(real_kinds /= [real32, real64, real128])) error stop 2

    i1 = [1_int8, -3_int8, 0_int8, 7_int8]
    i2 = [100_int16, 200_int16, 300_int16]
    i4 = [3, -1, 4]
    i8 = [huge(1_int64) - 5_int64, 2_int64]
    r4 = [0.5_real32, 0.25_real32]
    r8 = [1.5_real64, 2.25_real64]
    ! 2**-100 is lost next to 1 in real64 but not in real128.
    tiny128 = 2.0_real128**(-100)
    r16 = [1.0_real128, tiny128, 0.0_real128]
    c4 = [(1.0_real32, 2.0_real32), (0.0_real32, 0.0_real32), (3.0_real32, -1.0_real32)]
    c8 = [(1.5_real64, 0.0_real64), (0.0_real64, -2.0_real64)]

    if (total(i1) /= 5_int8 .or. kind(total(i1)) /= int8) error stop 10
    if (total(i2) /= 600_int16 .or. kind(total(i2)) /= int16) error stop 11
    if (total(i4) /= 6 .or. kind(total(i4)) /= int32) error stop 12
    if (total(i8) /= huge(1_int64) - 3_int64 .or. kind(total(i8)) /= int64) error stop 13
    if (total(r4) /= 0.75_real32 .or. kind(total(r4)) /= real32) error stop 14
    if (total(r8) /= 3.75_real64 .or. kind(total(r8)) /= real64) error stop 15
    if (total(r16) /= 1.0_real128 + tiny128 .or. total(r16) == 1.0_real128) error stop 16
    if (kind(total(r16)) /= real128) error stop 17
    if (total(c4) /= (4.0_real32, 1.0_real32) .or. kind(total(c4)) /= real32) error stop 18
    if (total(c8) /= (1.5_real64, -2.0_real64) .or. kind(total(c8)) /= real64) error stop 19
    if (total{integer(int16)}(i2(2:3)) /= 500_int16) error stop 20
    if (total{complex(real64)}(c8(2:2)) /= (0.0_real64, -2.0_real64)) error stop 21

    if (total_explicit(i1) /= total(i1)) error stop 22
    if (total_explicit(i2) /= total(i2)) error stop 23
    if (total_explicit(i4) /= total(i4)) error stop 24
    if (total_explicit(i8) /= total(i8)) error stop 25
    if (total_explicit(r4) /= total(r4)) error stop 26
    if (total_explicit(r8) /= total(r8)) error stop 27
    if (total_explicit(r16) /= total(r16)) error stop 28
    if (total_explicit(c4) /= total(c4)) error stop 29
    if (total_explicit{complex(real64)}(c8) /= total(c8)) error stop 30

    call check_sums(provider, i1, i2, i4, i8, r4, r8, r16)
    call check_counts(provider, i1, i2, i8, r4, r16, c4, c8)
    call check_largest(provider, i1, i2, i8, r8, r16)
    print '(a)', 'traits_kind_sets_01: ok'

contains

    subroutine check_sums(summer, i1, i2, i4, i8, r4, r8, r16)
        class(ISum),    intent(in) :: summer
        integer(int8),  intent(in) :: i1(:)
        integer(int16), intent(in) :: i2(:)
        integer(int32), intent(in) :: i4(:)
        integer(int64), intent(in) :: i8(:)
        real(real32),   intent(in) :: r4(:)
        real(real64),   intent(in) :: r8(:)
        real(real128),  intent(in) :: r16(:)
        if (summer%sum(i1) /= 5_int8 .or. kind(summer%sum(i1)) /= int8) error stop 40
        if (summer%sum(i2) /= 600_int16 .or. kind(summer%sum(i2)) /= int16) error stop 41
        if (summer%sum(i4(3:1:-1)) /= 6) error stop 42
        if (summer%sum{integer(int64)}(i8) /= huge(1_int64) - 3_int64) error stop 43
        if (summer%sum(r4) /= 0.75_real32 .or. kind(summer%sum(r4)) /= real32) error stop 44
        if (summer%sum(r8(2:2)) /= 2.25_real64) error stop 45
        if (summer%sum{real(real128)}(r16) /= 1.0_real128 + r16(2)) error stop 46
        if (kind(summer%sum(r16)) /= real128) error stop 47
    end subroutine check_sums

    subroutine check_counts(counter, i1, i2, i8, r4, r16, c4, c8)
        class(ICount),   intent(in) :: counter
        integer(int8),   intent(in) :: i1(:)
        integer(int16),  intent(in) :: i2(:)
        integer(int64),  intent(in) :: i8(:)
        real(real32),    intent(in) :: r4(:)
        real(real128),   intent(in) :: r16(:)
        complex(real32), intent(in) :: c4(:)
        complex(real64), intent(in) :: c8(:)
        if (counter%nonzero(i1) /= 3) error stop 50
        if (counter%nonzero(i2(1:0)) /= 0) error stop 51
        if (counter%nonzero(i8) /= 2) error stop 52
        if (counter%nonzero(r4) /= 2) error stop 53
        if (counter%nonzero(r16) /= 2) error stop 54
        if (counter%nonzero(c4) /= 2) error stop 55
        if (counter%nonzero{complex(real64)}(c8) /= 2) error stop 56
        if (counter%nonzero(c4(2:2)) /= 0) error stop 57
    end subroutine check_counts

    subroutine check_largest(maximum, i1, i2, i8, r8, r16)
        class(IMax),    intent(in) :: maximum
        integer(int8),  intent(in) :: i1(:)
        integer(int16), intent(in) :: i2(:)
        integer(int64), intent(in) :: i8(:)
        real(real64),   intent(in) :: r8(:)
        real(real128),  intent(in) :: r16(:)
        if (maximum%largest(i1) /= 7_int8 .or. kind(maximum%largest(i1)) /= int8) error stop 60
        if (maximum%largest(i2(3:1:-1)) /= 300_int16) error stop 61
        if (maximum%largest(i8) /= huge(1_int64) - 5_int64) error stop 62
        if (maximum%largest(r8) /= 2.25_real64) error stop 63
        if (maximum%largest{real(real128)}(r16) /= 1.0_real128) error stop 64
        if (maximum%largest(r16(2:3)) /= r16(2)) error stop 65
    end subroutine check_largest
end program traits_kind_sets_01
