module traits_runtime_owning_separate_01_consumer_m
    use traits_runtime_owning_separate_01_contracts_m
    implicit none
    class(IValue), allocatable :: held
contains
    subroutine hold(view)
        class(IValue), intent(in) :: view
        held = view
    end subroutine
    function snapshot_value(view) result(r)
        class(IValue), intent(in) :: view
        class(IValue), allocatable :: owner, copy
        integer :: r, check
        allocate(owner, source=view)
        copy = owner
        deallocate(owner)
        call copy%read_value(check)
        r = copy%value()
        if (r /= check) error stop 1
    end function
end module
