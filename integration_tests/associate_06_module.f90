module associate_06_stdlib_string_type

contains

    elemental subroutine unused_dummy_argument(dummy)
        class(*), intent(in) :: dummy
        associate(dummy => dummy); end associate
    end subroutine unused_dummy_argument

    subroutine read_formatted(v_list)
        integer, intent(in) :: v_list(:)

        call unused_dummy_argument(v_list)

    end subroutine

end module
