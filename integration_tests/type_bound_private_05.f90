program type_bound_private_05
    ! Separate compilation of a type-bound generic inherited by an extension
    ! declared in another module, whose same-named binding does not override
    ! the parent's private specific (#16272).
    use type_bound_private_05_base_m, only: base_t, run
    use type_bound_private_05_child_m, only: child_t
    implicit none
    type(child_t) :: c
    class(base_t), allocatable :: p

    call run(c)
    if (c%calls /= 2) error stop
    if (c%child_calls /= 0) error stop

    call c%resolve()
    if (c%calls /= 3) error stop
    if (c%child_calls /= 0) error stop

    allocate(child_t :: p)
    call run(p)
    call p%resolve()
    if (p%calls /= 3) error stop
    select type (p)
    type is (child_t)
        if (p%child_calls /= 0) error stop
    class default
        error stop
    end select
    print *, c%calls, p%calls
end program type_bound_private_05
