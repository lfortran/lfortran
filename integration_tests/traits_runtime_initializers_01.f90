! Runtime views of traits with initializer requirements (traits.tex, "Class
! specifier using trait combinations"): class(IAdmissible), where
! IAdmissible extends ICastable + IPrintable, and class(ICastable +
! IPrintable). The initializer of ICastable is required for conformance and
! still constructs values through the type and a generic T(n), but it is not
! callable through an object: the views dispatch only the messages, and a
! view of ICastable alone has no slot and is inspected with SELECT TYPE.
! The contracts and the conformance are separately compiled modules.
program traits_runtime_initializers_01
    use traits_runtime_initializers_01_contracts, only: ICastable, IPrintable, &
        IAdmissible, cast, show, show_admissible
    use traits_runtime_initializers_01_types, only: Admitted, last_output, last_input
    implicit none
    class(IAdmissible), allocatable :: obj
    class(ICastable + IPrintable), allocatable :: pair
    class(ICastable), allocatable :: castable, copy
    type(Admitted) :: value

    value = Admitted(7)
    if (value%n /= 70) error stop 1
    allocate(obj, source=value)
    call obj%output()
    if (last_output /= 70) error stop 2
    call obj%input('data.txt')
    if (last_input /= 78) error stop 3

    allocate(pair, source=cast{Admitted}(5))
    call pair%output()
    if (last_output /= 50) error stop 4
    call show(pair)
    if (last_output /= 50) error stop 5
    call show(obj)
    if (last_output /= 70) error stop 6
    call show_admissible(Admitted(2), 'abc')
    if (last_output /= 20 .or. last_input /= 23) error stop 7

    allocate(castable, source=value)
    copy = castable
    deallocate(castable)
    select type (concrete => copy)
    type is (Admitted)
        if (concrete%n /= 70) error stop 8
    class default
        error stop 9
    end select
    deallocate(obj, pair, copy)
    print '(a)', 'traits_runtime_initializers_01 ok'
end program traits_runtime_initializers_01
