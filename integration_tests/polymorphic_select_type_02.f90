program polymorphic_select_type_02
    integer :: a(3) = [1, 2, 3]
    call print_generic(a)

contains

    subroutine print_generic(generic)
        class(*), intent(in) :: generic(:)

        select type (generic)
        type is (integer)
            if (size(generic) /= 3) error stop 1
            if (any(generic /= [1, 2, 3])) error stop 2
        class default
            error stop 3
        end select
    end subroutine print_generic

end program polymorphic_select_type_02
