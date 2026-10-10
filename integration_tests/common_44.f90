program common_44
    use common_44_mod, only: bind
    use common_44_legacy_mod, only: set_value
    implicit none
    integer, pointer :: x
    call bind(x)
    call set_value()
    print *, x
    if (x /= 17) error stop
end program common_44
