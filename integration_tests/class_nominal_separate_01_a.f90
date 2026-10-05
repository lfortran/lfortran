module class_nominal_separate_01_a_m
    implicit none
    type :: Payload
        integer :: value
    end type Payload
contains
    subroutine accept_a(object)
        class(Payload), intent(in) :: object
        select type(object)
        type is(Payload)
            if (object%value /= 11) error stop 1
        class default
            error stop 2
        end select
    end subroutine accept_a
    subroutine accept_not_a(object)
        class(*), intent(in) :: object
        select type(object)
        type is(Payload)
            error stop 3
        class default
            continue
        end select
    end subroutine accept_not_a
end module class_nominal_separate_01_a_m
