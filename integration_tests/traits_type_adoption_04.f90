program traits_type_adoption_04
    use traits_type_adoption_04_provider, only: Open, Closed, Parent, &
        final_count, final_sum, receiver_finals, read_static, read_runtime
    use traits_type_adoption_04_consumer, only: via_parent, via_named, via_add
    implicit none
    type(Open), target :: ordinary
    type(Closed), target :: object
    class(Closed), pointer :: exact
    class(Parent), pointer :: ancestor
    integer :: direct, through_exact, through_parent, with_offset, without_offset

    exact => object
    ancestor => object
    direct = object%value()
    through_exact = exact%value()
    if (direct /= 19 .or. through_exact /= 19) error stop 1
    if (final_count /= 2 .or. final_sum /= 14) error stop 2
    if (via_parent(ordinary) /= 18) error stop 3

    through_parent = via_parent(ancestor)
    if (through_parent /= 19) error stop 4
    if (final_count /= 3 .or. final_sum /= 21) error stop 5
    if (read_static(object) /= 19) error stop 6
    if (read_runtime(object) /= 19) error stop 7
    if (final_count /= 5 .or. final_sum /= 35) error stop 8

    with_offset = via_named(ancestor, .true.)
    without_offset = via_named(ancestor, .false.)
    if (with_offset /= 39 .or. without_offset /= 36) error stop 9
    call via_add(ancestor)
    if (object%n /= 24 .or. object%bias /= 2) error stop 10
    if (via_parent(ancestor) /= 26) error stop 11
    if (final_count /= 6 .or. final_sum /= 42) error stop 12
    if (receiver_finals /= 0) error stop 13
    print *, "sealed ancestor dispatch: 19 39 36 26; finals: 6 42"
    nullify(exact, ancestor)
end program
