module module_1
    implicit none

    type t_1
        integer :: i
    end type t_1

    type t_2
        type(t_1), pointer :: type_2(:)
    end type t_2
contains
    subroutine sub_1(type_1, i)
        class(t_2), intent(inout) :: type_1
        integer, intent(in) :: i
        allocate(type_1%type_2(0))
        associate(target=>type_1%type_2(0)%i)
            target = i
        end associate
    end subroutine sub_1

    subroutine sub_2(progress)
        class(t_2), intent(inout), allocatable :: progress(:)
        allocate(progress(0)%type_2(0))
        associate(target => progress(0))
            target%type_2(0)%i = 345
        end associate
    end subroutine sub_2
end module module_1

program associate2
    use module_1
    implicit none
    type(t_2) :: type_1
    call sub_1(type_1, 123)
    print *, type_1%type_2(0)%i
end program associate2
