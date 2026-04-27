module select_type_poly_01_types
    implicit none

    type :: my_data
        integer :: x = 0
        integer, allocatable :: vals(:)
    contains
        final :: cleanup_my_data
    end type

    type :: my_other
        integer :: tag = 0
    end type

contains

    subroutine cleanup_my_data(this)
        type(my_data), intent(inout) :: this
        if (allocated(this%vals)) deallocate(this%vals)
    end subroutine

end module

module select_type_poly_01_mod
    use select_type_poly_01_types
    implicit none

contains

    subroutine process_scalar(input, res)
        class(*), intent(in) :: input
        integer, intent(out) :: res

        res = -1
        select type(source => input)
        type is (my_data)
            res = source%x
        type is (my_other)
            res = source%tag
        type is (integer)
            res = source
        end select
    end subroutine

end module

program select_type_poly_01
    use select_type_poly_01_mod
    use select_type_poly_01_types
    implicit none

    type(my_data) :: a
    type(my_other) :: b
    integer :: r

    a%x = 42
    b%tag = 99

    call process_scalar(a, r)
    if (r /= 42) error stop

    call process_scalar(b, r)
    if (r /= 99) error stop

    call process_scalar(7, r)
    if (r /= 7) error stop

    print *, "ok"
end program
