! Compiled against the contracts only: it reads, mutates and resets objects
! of providers it does not know through trait views and a pointer.
module traits_receiver_intent_separate_01_consumer_m
    use traits_receiver_intent_separate_01_contracts_m, only: ICounter
    implicit none
contains
    integer function observe(object)
        class(ICounter), intent(in) :: object
        observe = object%value()
    end function observe

    subroutine exercise(object, start, step, bumped, seed, restarted)
        class(ICounter), intent(inout) :: object
        integer, intent(in) :: start, step, bumped, seed, restarted
        integer :: taken
        if (observe(object) /= start) error stop 701
        call object%bump(step)
        if (observe(object) /= bumped) error stop 702
        taken = object%take()
        if (taken /= bumped .or. observe(object) /= 0) error stop 703
        call object%reset(seed=seed)
        if (observe(object) /= restarted) error stop 704
    end subroutine exercise

    subroutine choose(choice, first, second)
        integer, intent(in) :: choice
        class(ICounter), intent(inout) :: first, second
        if (choice == 0) then
            call exercise(first, 11, 3, 14, 4, 4)
        else
            call exercise(second, 30, 5, 45, 4, 8)
        end if
    end subroutine choose

    ! An intent(in) pointer fixes the association, not the target.
    subroutine nudge(object, by)
        class(ICounter), pointer, intent(in) :: object
        integer, intent(in) :: by
        call object%bump(by=by)
    end subroutine nudge
end module traits_receiver_intent_separate_01_consumer_m
