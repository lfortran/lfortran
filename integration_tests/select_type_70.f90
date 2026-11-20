module select_type_70_m
    implicit none

    type, abstract :: abs_base
        integer :: k = 0
    end type

    type, extends(abs_base) :: child
        integer :: j = 0
    end type

    type :: base
        integer :: k = 0
    end type

    type :: runner
        integer :: offset = 100
    contains
        procedure :: run => runner_run
    end type

contains

    subroutine run_base(t, out)
        class(base), pointer, intent(in) :: t
        integer, intent(out) :: out
        if (.not. associated(t)) error stop
        out = t%k
    end subroutine

    subroutine run_abs_base(t, out)
        class(abs_base), pointer, intent(in) :: t
        integer, intent(out) :: out
        if (.not. associated(t)) error stop
        out = t%k
    end subroutine

    subroutine run_child(t, out_k, out_j)
        class(child), pointer, intent(in) :: t
        integer, intent(out) :: out_k, out_j
        if (.not. associated(t)) error stop
        out_k = t%k
        out_j = t%j
    end subroutine

    subroutine runner_run(self, t, out)
        class(runner), intent(in) :: self
        class(abs_base), pointer, intent(in) :: t
        integer, intent(out) :: out
        if (.not. associated(t)) error stop
        out = self%offset + t%k
    end subroutine

    subroutine check_unassociated(t, is_assoc)
        class(base), pointer, intent(in) :: t
        logical, intent(out) :: is_assoc
        is_assoc = associated(t)
    end subroutine

    subroutine test_dummy_selector(p, r)
        class(abs_base), pointer, intent(in) :: p
        type(runner), intent(in) :: r
        integer :: out_k, out_j, out_r
        logical :: matched

        matched = .false.
        select type (p)
        class is (child)
            matched = .true.
            call run_abs_base(p, out_k)
            if (out_k /= 11) error stop
            call run_child(p, out_k, out_j)
            if (out_k /= 11 .or. out_j /= 22) error stop
            call r%run(p, out_r)
            if (out_r /= 111) error stop
        end select
        if (.not. matched) error stop

        matched = .false.
        select type (p)
        type is (child)
            matched = .true.
            call run_abs_base(p, out_k)
            if (out_k /= 11) error stop
            call run_child(p, out_k, out_j)
            if (out_k /= 11 .or. out_j /= 22) error stop
            call r%run(p, out_r)
            if (out_r /= 111) error stop
        end select
        if (.not. matched) error stop
    end subroutine

end module

program select_type_70
    use select_type_70_m
    implicit none

    class(base), pointer :: q, q_null
    class(abs_base), pointer :: p
    type(runner) :: r
    integer :: out
    logical :: matched, is_assoc

    allocate(q)
    q%k = 7

    matched = .false.
    select type (q)
    class is (base)
        matched = .true.
        call run_base(q, out)
        if (out /= 7) error stop
    end select
    if (.not. matched) error stop

    matched = .false.
    select type (q)
    type is (base)
        matched = .true.
        call run_base(q, out)
        if (out /= 7) error stop
    end select
    if (.not. matched) error stop

    q_null => null()
    matched = .false.
    is_assoc = .true.
    select type (q_null)
    class is (base)
        matched = .true.
        call check_unassociated(q_null, is_assoc)
    end select
    if (.not. matched .or. is_assoc) error stop

    allocate(child :: p)
    select type (p)
    type is (child)
        p%k = 11
        p%j = 22
    end select
    call test_dummy_selector(p, r)

    deallocate(q)
    deallocate(p)
    print *, "ok"
end program
