! A client of the private owner module of traits_generic_type_04 that uses
! all of it and names no instance type: only the module's public procedures
! arrive, and the runtime view the module made works here.
program traits_generic_type_04_use_all
    use traits_generic_type_04_contracts, only: IAverager
    use traits_generic_type_04_owner
    implicit none
    class(IAverager), allocatable :: av
    av = make_view(2)
    if (av%average([1, 2, 3, 4, 5]) /= 3) error stop
    print '(a)', "traits_generic_type_04_use_all: owner entities only"
end program traits_generic_type_04_use_all
