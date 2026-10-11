! A runtime view of a value of an instance the owner module of
! traits_generic_type_04 owns: the module's conformances of its instances are
! its own, so this client sees none.
program traits_generic_type_04_use_view
    use traits_generic_type_04_contracts, only: IAverager
    use traits_generic_type_04_owner
    implicit none
    class(IAverager), allocatable :: av
    av = make_avg()
    print *, av%average([1, 2, 3])
end program traits_generic_type_04_use_view
