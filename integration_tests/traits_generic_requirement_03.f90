! Generic procedures calling generic trait methods through their type
! parameters, compiled separately from their conformances and clients, and
! reached through renamed ONLY imports. A local generic forwards to the
! separately compiled one, so its own requirement publishes the specialization
! the loaded body uses. GFortran does not accept this LFortran extension.
module traits_generic_requirement_03_local
    use traits_generic_requirement_03_contracts, only: ISum, INumeric
    use traits_generic_requirement_03_generic, only: doubled => twice_sum
    implicit none
    private
    public :: local_forward
contains
    function local_forward{ISum :: Local, INumeric :: E}(a, x) result(s)
        type(Local), intent(in) :: a
        type(E),     intent(in) :: x(:)
        type(E)                 :: s
        s = doubled(a, x)
    end function local_forward
end module traits_generic_requirement_03_local

program traits_generic_requirement_03
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_requirement_03_provider, only: Simple => SimpleSum, ScaledSum, &
        Value, Offset
    use traits_generic_requirement_03_generic, only: doubled => twice_sum, &
        forward_sum, apply_twice
    use traits_generic_requirement_03_local, only: local_forward
    implicit none
    integer :: xi(3) = [1, 2, 3]
    real(real64) :: xr(2) = [0.5_real64, 2.0_real64]

    if (doubled(Simple(), xi) /= 12) error stop 1
    if (doubled(ScaledSum(3), xr) /= 15.0_real64) error stop 2
    if (forward_sum(ScaledSum(2), xi) /= 36) error stop 3
    if (forward_sum{Simple, real(real64)}(Simple(), xr) /= 7.5_real64) error stop 4
    if (apply_twice(Offset(10), Value(7)) /= 34) error stop 5
    if (local_forward(ScaledSum(5), xi) /= 60) error stop 6
    if (local_forward(Simple(), xr) /= 5.0_real64) error stop 7
    print '(a)', 'traits_generic_requirement_03 ok'
end program traits_generic_requirement_03
