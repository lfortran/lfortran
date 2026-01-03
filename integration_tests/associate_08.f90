module associate_08_module_1
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
        allocate(type_1%type_2(1))
        associate(target=>type_1%type_2(1)%i)
            target = i
        end associate
    end subroutine sub_1

    subroutine sub_2(progress)
        class(t_2), intent(inout), allocatable :: progress(:)
        allocate(progress(1))
        allocate(progress(1)%type_2(1))
        associate(target => progress(1))
            target%type_2(1)%i = 345
        end associate
    end subroutine sub_2
end module

program associate2
    use associate_08_module_1
    implicit none
    type(t_2) :: type_1
    class(t_2), allocatable :: t_2_array(:)
    call sub_1(type_1, 123)
    if (type_1%type_2(1)%i /= 123) error stop
    print *, type_1%type_2(1)%i

    ! TODO: Fix when implementing class arrays
    ! call sub_2(t_2_array)
    ! print *, t_2_array(1)%type_2(1)%i
end program associate2
