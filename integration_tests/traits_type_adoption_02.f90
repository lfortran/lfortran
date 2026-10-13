program traits_type_adoption_02
    use traits_type_adoption_02_facade, only: Concrete, Base, IValue, IExtra
    use traits_type_adoption_02_child, only: Again => Child
    use traits_type_adoption_02_consumer, only: static_read, read_subset
    implicit none
    type(Concrete), target :: x
    class(Base), pointer :: ordinary
    class(IValue + IExtra), allocatable :: owner
    class(IExtra + IValue), pointer :: view

    if (static_read(x) /= 39) error stop
    if (read_subset(x) /= 23) error stop
    ordinary => x
    if (ordinary%legacy() /= 4 .or. ordinary%value() /= 5) error stop
    view => x
    if (read_subset(view) /= 23) error stop
    allocate(owner, source=x)
    if (read_subset(owner) /= 23) error stop
    select type (owner)
    type is (Again)
        if (owner%bonus /= 11) error stop
    class default
        error stop
    end select
    x%seed = 8
    if (read_subset(view) /= 29) error stop
    if (read_subset(owner) /= 23) error stop
    nullify(view, ordinary)
    deallocate(owner)
    print *, "abstract adoption: 39 23 29; legacy: 4"
end program
