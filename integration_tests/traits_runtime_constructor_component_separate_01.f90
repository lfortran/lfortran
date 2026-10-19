! Default structure constructors with runtime trait component values across
! separately compiled modules: the consumer is compiled without the provider of
! the conforming types, one of them private.
program traits_runtime_constructor_component_separate_01
    use traits_runtime_constructor_component_separate_01_contracts_m, only: &
        IShape, IName, Holder, Duo
    use traits_runtime_constructor_component_separate_01_provider_m, only: &
        Circle, make_square, finals, circle_holder
    use traits_runtime_constructor_component_separate_01_consumer_m, only: &
        wrap, wrap_owner, pair, twice, total, copy_and_scale
    implicit none
    type(Holder) :: h, copy
    type(Duo) :: d
    type(Circle) :: c
    class(IShape), allocatable :: owner, none
    class(IShape+IName), allocatable :: sq
    class(*), allocatable :: res

    c%r = 2
    h = wrap(c, 5)
    if (abs(total(h) - 12) > 1e-5 .or. h%tag /= 5) error stop 1
    c%r = 3
    if (abs(total(h) - 12) > 1e-5) error stop 2
    h = Holder(c)
    if (abs(total(h) - 27) > 1e-5 .or. h%tag /= 0) error stop 3
    h = circle_holder(1.0)
    if (abs(total(h) - 3) > 1e-5 .or. h%tag /= 1) error stop 4
    if (finals /= 5) error stop 16
    call copy_and_scale(h, copy)
    if (abs(total(copy) - 3) > 1e-5 .or. copy%tag /= 11) error stop 5
    if (abs(total(h) - 12) > 1e-5) error stop 6

    owner = Circle(1.0)
    res = wrap_owner(owner)
    select type (res)
    type is (Holder)
        if (abs(total(res) - 3) > 1e-5 .or. res%tag /= 2) error stop 7
    class default
        error stop 8
    end select
    res = wrap_owner(none)
    select type (res)
    type is (Holder)
        if (allocated(res%tobj) .or. res%tag /= 2) error stop 9
        if (finals /= 7) error stop 17
    class default
        error stop 10
    end select

    sq = make_square(2.0, "sq")
    d = pair(owner, sq)
    call sq%scale(3.0)
    if (abs(d%first%area() - 3) > 1e-5 .or. abs(d%named%area() - 4) > 1e-5) error stop 11
    if (d%named%name() /= "sq" .or. abs(sq%area() - 36) > 1e-5) error stop 12
    d = twice(sq)
    if (abs(d%first%area() - 36) > 1e-5 .or. d%named%name() /= "sq") error stop 13
    call d%first%scale(0.5)
    if (abs(d%first%area() - 9) > 1e-5 .or. abs(d%named%area() - 36) > 1e-5) error stop 14
    d = Duo(named=sq)
    if (allocated(d%first) .or. d%named%name() /= "sq") error stop 15
    if (finals /= 17) error stop 18
    print '(a)', 'traits_runtime_constructor_component_separate_01 ok'
end program
