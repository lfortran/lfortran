! A client of a private module that owns generic derived type instances uses
! them only through the module's public procedures: the values it receives,
! their bindings and the runtime views the module made. The module's
! instances, procedure copies, initializers and conformances stay its own.
program traits_generic_type_04
    use traits_generic_type_04_contracts, only: IAverager
    use traits_generic_type_04_owner, only: make_pair, pair_total, make_view, make_avg
    implicit none
    class(IAverager), allocatable :: av
    integer :: checks
    checks = 0
    p := make_pair()
    call check(pair_total(p, [1, 2, 3, 4, 5]) == 15)
    call check(p%sum([1, 2, 3, 4, 5]) == 15)
    call check(abs(p%sum([1.d0, 2.d0, 3.d0]) - 6.d0) < 1.d-12)
    av = make_view(1)
    call check(av%average([1, 2, 3, 4, 5]) == 3)
    av = make_view(2)
    call check(abs(av%average([1.d0, 2.d0, 3.d0, 4.d0, 5.d0]) - 3.d0) < 1.d-12)
    a := make_avg()
    call check(a%average([2, 4, 6]) == 4)
    print '(a, i0)', "traits_generic_type_04 checks: ", checks
contains
    subroutine check(condition)
        logical, intent(in) :: condition
        if (.not. condition) error stop
        checks = checks + 1
    end subroutine check
end program traits_generic_type_04
