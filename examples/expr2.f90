module debug
    use debug_2
    implicit none

    type, abstract, extends(t_2) :: t
    contains
        procedure :: sub
    end type t

contains
    subroutine sub(arg)
        class(t), intent(inout) :: arg
        call sub1(arg)
    contains
        subroutine sub1(arg)
            class(t), intent(inout) :: arg
        end subroutine sub1
    end subroutine sub
end module debug
