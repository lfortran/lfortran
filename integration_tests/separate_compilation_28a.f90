subroutine string_handler(name, info)
    character(*), intent(in) :: name
    integer, intent(in) :: info
    print *, "Handler got: '", name, "' info=", info
end subroutine
