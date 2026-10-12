! LFortran traits extension test; GFortran does not accept this syntax.
! The real(real128) member of the kind wildcard sets of traits_kind_sets_01,
! called statically and through the closed runtime slots of the same
! separately compiled contracts and providers. It is a test of its own because
! LLVM 7 and 8 miscompile real(16) operations (ci/test_llvm_integration.sh).
program traits_kind_sets_02
    use, intrinsic :: iso_fortran_env, only: real128
    use traits_kind_sets_01_contracts, only: ISum, ICount, IMax
    use traits_kind_sets_01_providers, only: Summer, total, total_explicit
    implicit none
    real(real128) :: r16(3), tiny128
    type(Summer)  :: provider

    ! 2**-100 is lost next to 1 in real64 but not in real128.
    tiny128 = 2.0_real128**(-100)
    r16 = [1.0_real128, tiny128, 0.0_real128]

    if (total(r16) /= 1.0_real128 + tiny128 .or. total(r16) == 1.0_real128) error stop 16
    if (kind(total(r16)) /= real128) error stop 17
    if (total_explicit(r16) /= total(r16)) error stop 28

    call check_sums(provider, r16)
    call check_counts(provider, r16)
    call check_largest(provider, r16)
    print '(a)', 'traits_kind_sets_02: ok'

contains

    subroutine check_sums(summer, r16)
        class(ISum),   intent(in) :: summer
        real(real128), intent(in) :: r16(:)
        if (summer%sum{real(real128)}(r16) /= 1.0_real128 + r16(2)) error stop 46
        if (kind(summer%sum(r16)) /= real128) error stop 47
    end subroutine check_sums

    subroutine check_counts(counter, r16)
        class(ICount), intent(in) :: counter
        real(real128), intent(in) :: r16(:)
        if (counter%nonzero(r16) /= 2) error stop 54
    end subroutine check_counts

    subroutine check_largest(maximum, r16)
        class(IMax),   intent(in) :: maximum
        real(real128), intent(in) :: r16(:)
        if (maximum%largest{real(real128)}(r16) /= 1.0_real128) error stop 64
        if (maximum%largest(r16(2:3)) /= r16(2)) error stop 65
    end subroutine check_largest
end program traits_kind_sets_02
