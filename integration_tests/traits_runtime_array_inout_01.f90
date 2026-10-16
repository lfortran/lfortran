! LFortran traits extension test; GFortran does not accept this syntax.
! The reported module, unchanged in traits_runtime_array_inout_01_mytype.f90:
! ICoeffs%update takes, beside its intent(inout) receiver, a borrowed
! class(IEstimates), intent(inout) view of the empty marker trait IEstimates
! and an ordinary real(8), intent(inout) assumed-shape array beta(:), and the
! type-bound MyType%coefficients calls it with the allocatable runtime trait
! components of MyType and the fixed-size module array beta(12). Through a
! MyType variable and a class(MyType) owner, with two implementations of each
! trait, every call updates all twelve elements of the module's beta in
! place, its own receiver and the borrowed estimates, which keep their
! dynamic types.
program traits_runtime_array_inout_01
    use mytype_mod, only: MyType, beta
    use traits_runtime_array_inout_01_providers, only: Prior, Tally, &
        Coeffs, Damped
    implicit none
    type(MyType) :: t
    class(MyType), allocatable :: owner
    real(8) :: expected(12), total
    integer :: i, k

    allocate(Coeffs :: t%coeffs)
    allocate(Prior :: t%estimates)
    beta = [(real(i, 8), i = 1, 12)]
    expected = beta
    do k = 1, 3
        call t%coefficients()
        expected = [(2*expected(i) + i, i = 1, 12)]
        call check_beta(k)
    end do
    call check_first(3)

    ! The other implementations of both traits, through a class(MyType) owner.
    allocate(owner)
    allocate(Damped :: owner%coeffs)
    allocate(Tally :: owner%estimates)
    total = 0
    do k = 1, 2
        total = total + sum(expected)
        call owner%coefficients()
        expected = 0.5d0*expected - k
        call check_beta(3 + k)
    end do
    select type (c => owner%coeffs)
    type is (Damped)
        if (c%updates /= 2 .or. c%factor /= 0.5d0) error stop 20
    class default
        error stop 21
    end select
    select type (e => owner%estimates)
    type is (Tally)
        if (e%seen /= 2 .or. e%total /= total) error stop 22
    class default
        error stop 23
    end select

    ! The first object is unchanged by the second's calls and still works.
    call check_first(3)
    call t%coefficients()
    expected = [(2*expected(i) + i, i = 1, 12)]
    call check_beta(6)
    call check_first(4)
    print '(a)', 'traits_runtime_array_inout_01 ok'

contains

    subroutine check_beta(step)
        integer, intent(in) :: step
        if (any(beta /= expected)) then
            print '(a, i0)', 'unexpected module beta after call ', step
            print *, beta
            error stop 1
        end if
    end subroutine check_beta

    subroutine check_first(calls)
        integer, intent(in) :: calls
        select type (c => t%coeffs)
        type is (Coeffs)
            if (c%updates /= calls) error stop 10
        class default
            error stop 11
        end select
        select type (e => t%estimates)
        type is (Prior)
            if (e%seen /= calls) error stop 12
        class default
            error stop 13
        end select
    end subroutine check_first
end program traits_runtime_array_inout_01
