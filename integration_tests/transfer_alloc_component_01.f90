module transfer_alloc_component_01_m
    type key_type
        integer(1), allocatable :: value(:)
    end type
contains
    subroutine get_char_key(key, value)
        type(key_type), intent(in) :: key
        character(:), allocatable, intent(out) :: value
        integer :: n
        n = size(key%value)
        allocate(character(len=n) :: value)
        value(1:n) = transfer(key%value, value)
    end subroutine
end module

program transfer_alloc_component_01
    use transfer_alloc_component_01_m
    type(key_type) :: key
    character(:), allocatable :: str
    integer(1) :: bytes(11)
    bytes = transfer("initial key", bytes)
    allocate(key%value(11))
    key%value = bytes
    call get_char_key(key, str)
    if (str /= "initial key") error stop 1
end program
