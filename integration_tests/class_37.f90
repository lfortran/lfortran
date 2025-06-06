module stdlib_hashmap_wrappers_class_37

    implicit none

    type :: other_type
        class(*), allocatable :: value
    end type other_type

end module stdlib_hashmap_wrappers_class_37


program class_37

    use stdlib_hashmap_wrappers_class_37
    implicit none

    type(other_type) :: other
    integer :: val = 1
    call get_other_open_data( other, val )

    print *, val
    if (val /= 2) error stop

contains

    subroutine get_other_open_data( other, val )
        type(other_type), intent(out)           :: other
        integer,intent(inout) :: val
        val = 2
    end subroutine get_other_open_data

end program