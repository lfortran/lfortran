 module class_89_mod
    implicit none

    type :: other_type
        class(*), allocatable :: value
    end type other_type

contains

    subroutine copy_other(src, dest)
        type(other_type), intent(in)  :: src
        type(other_type), intent(out) :: dest
        allocate(dest%value, source=src%value)
    end subroutine copy_other

end module class_89_mod

program class_89
    use class_89_mod, only : copy_other, other_type
    implicit none

    type :: dummy_type
        integer :: val(15)
    end type dummy_type

    type(other_type) :: other_in, other_out
    type(dummy_type) :: dummy_val
    integer :: i
    dummy_val%val = [(i, i=1,15)]
    allocate(other_in%value, source=dummy_val)

    call copy_other(other_in, other_out)

    select type(p => other_out%value)
    type is (dummy_type)
        print *, p%val
        if (any(p%val /= dummy_val%val)) error stop
    end select
end program class_89