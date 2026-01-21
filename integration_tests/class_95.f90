program class_95
    implicit none

    character(len=5) :: arr(2) = ["hello", "world"]

    call driver(arr)

contains

    subroutine driver(generic)
        class(*) :: generic(:)

        select type (generic)
        type is (character(len=*))
            call get_fixedarray_fixed_length_c(generic)
        class default
            print *, "Unsupported type"
        end select
    end subroutine driver


    subroutine get_fixedarray_fixed_length_c(x)
        character(len=5), intent(in) :: x(2)
        if (x(1) /= "hello") error stop
        if (x(2) /= "world") error stop
    end subroutine get_fixedarray_fixed_length_c

end program class_95
