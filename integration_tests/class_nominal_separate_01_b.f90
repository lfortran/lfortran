module class_nominal_separate_01_b_m
    use class_nominal_separate_01_a_m, only: accept_not_a
    implicit none
    type :: Payload
        integer :: value, padding
    end type Payload
contains
    subroutine exercise_b()
        type(Payload) :: object
        object%value = 29
        object%padding = -99
        call accept_not_a(object)
    end subroutine exercise_b
end module class_nominal_separate_01_b_m
