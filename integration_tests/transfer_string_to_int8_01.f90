module transfer_string_to_int8_01_m
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none

    integer, parameter :: bytes_char = 1

    type :: key_type
        integer(int8), allocatable :: value(:)
    end type key_type

contains

    subroutine set_char_key(key, value)
        type(key_type), intent(out) :: key
        character(*), intent(in)    :: value

        allocate(key%value(bytes_char * len(value)))
        key%value = transfer(value, key%value, bytes_char * len(value))
    end subroutine set_char_key

end module transfer_string_to_int8_01_m

program transfer_string_to_int8_01
    use transfer_string_to_int8_01_m, only: key_type, set_char_key
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none

    type(key_type) :: key

    call set_char_key(key, "abc")
    if (size(key%value) /= 3) error stop

    if (key%value(1) /= int(iachar('a'), int8)) error stop
    if (key%value(2) /= int(iachar('b'), int8)) error stop
    if (key%value(3) /= int(iachar('c'), int8)) error stop
end program transfer_string_to_int8_01
