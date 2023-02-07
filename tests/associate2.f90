
module module_1
    implicit none

    type t_1
        integer :: x
    end type t_1

    type t_2
        type(t_1), pointer :: ptr => null()
    end type t_2

end module module_1

module module_2
    use module_1, only: t_2

    implicit none

    type t_3
        type(t_2), pointer :: type_1(:)
    end type t_3

contains

    subroutine sub_1(progress)
        class(t_3), intent(inout) :: progress
        associate(target=>progress%type_1(0)%ptr)
            print *
        end associate
    end subroutine sub_1
end module module_2
