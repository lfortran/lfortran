module module_character_abi
    implicit none
contains
    subroutine verify_module_character(value, status)
        character(len=*), intent(in) :: value
        integer, intent(out) :: status

        if (len_trim(value) == 6) then
            status = 0
        else
            status = 1
        end if
    end subroutine verify_module_character
end module module_character_abi

program implicit_interface_character_abi_01
    use module_character_abi, only: verify_module_character
    implicit none
    integer :: status
    character(len=5), external :: verify_character_result
    character(len=5) :: result

    status = -1
    call verify_one_character('N', status)
    if (status /= 0) error stop "single CHARACTER ABI mismatch"

    status = -1
    call verify_two_characters('N', 42, 'Z', status)
    if (status /= 0) error stop "multiple CHARACTER ABI mismatch"

    status = -1
    call verify_no_character(42, status)
    if (status /= 0) error stop "non-CHARACTER ABI mismatch"
    result = verify_character_result(42, 'Z')
    if (result /= 'R42Z!') error stop "CHARACTER result ABI mismatch"


    status = -1
    call verify_module_character('MODULE ', status)
    if (status /= 0) error stop "module CHARACTER descriptor mismatch"

    print *, "implicit external CHARACTER ABI verified"
end program implicit_interface_character_abi_01
