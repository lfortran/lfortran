module stdlib_string_type_struct_type_06

contains

    elemental subroutine unused_dummy_argument(dummy)
        class(*), intent(in) :: dummy
        associate(dummy => dummy); end associate
    end subroutine unused_dummy_argument

end module stdlib_string_type_struct_type_06

program struct_type_06
    use stdlib_string_type_struct_type_06

    implicit none
    call unused_dummy_argument(42)
    call unused_dummy_argument("Hello, World!")
    call unused_dummy_argument([1, 2, 3, 4, 5])

    print *, "Program end!"
end program
