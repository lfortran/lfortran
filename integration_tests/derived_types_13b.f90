module debug_2
    use debug_1
    implicit none
    type t_2
        type(t_1) :: operations
    end type t_2
contains
    subroutine sub_1()
        type(t_2) :: type
        call type%operations%compute(5.0)
    end subroutine sub_1
end module debug_2
