program traits_runtime_owning_separate_01
    use traits_runtime_owning_separate_01_consumer_m, only: held, hold, snapshot_value
    use traits_runtime_owning_separate_01_types_m, only: shared_finalizations
    use traits_runtime_owning_separate_01_a_m, only: left, setup_a
    use traits_runtime_owning_separate_01_b_m, only: right, other, setup_b, other_finalizations
    implicit none
    integer :: before_shared, before_other
    call setup_a()
    call setup_b()
    if (snapshot_value(left) /= 107) error stop 2
    if (snapshot_value(right) /= 207) error stop 3
    if (snapshot_value(other) /= 307) error stop 4
    if (command_argument_count() == 0) then
        call hold(left)
        if (held%value() /= 107) error stop 5
        call hold(right)
        if (held%value() /= 207) error stop 6
    else
        call hold(right)
        if (held%value() /= 207) error stop 7
        call hold(left)
        if (held%value() /= 107) error stop 8
    end if
    ! Identical nominal dynamic type but a different selected conformance.
    held = left
    if (held%value() /= 107) error stop 9
    held = right
    if (held%value() /= 207) error stop 10
    before_shared = shared_finalizations
    before_other = other_finalizations
    deallocate(held)
    if (shared_finalizations /= before_shared + 1) error stop 11
    if (other_finalizations /= before_other) error stop 12
    held = left
    ! Same-spelled, layout-equal, but nominally distinct payload and finalizer.
    held = other
    if (held%value() /= 307) error stop 13
    before_shared = shared_finalizations
    before_other = other_finalizations
    deallocate(held)
    if (shared_finalizations /= before_shared) error stop 14
    if (other_finalizations /= before_other + 1) error stop 15
    if (allocated(held)) error stop 16
    deallocate(left, right, other)
end program
