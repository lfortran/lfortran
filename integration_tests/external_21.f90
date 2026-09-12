module external_21_priv
    implicit none
    private
    ! A PRIVATE external declaration. Its accessibility must stay local to this
    ! module: it must not make the same name private in `external_21_pub`
    ! below, which has no `private` statement.
    character(len=80), external :: get_libvers
end module external_21_priv

module external_21_pub
    implicit none
    character(len=80), external :: get_libvers
end module external_21_pub

program external_21
    use external_21_pub
    implicit none
    character(len=80) :: v
    ! `get_libvers` is PUBLIC in external_21_pub, so use association must
    ! import it here.
    v = get_libvers()
    if (trim(v) /= "version 1.2.3") error stop "wrong version string"
    print *, trim(v)
end program external_21

function get_libvers() result(v)
    implicit none
    character(len=80) :: v
    v = "version 1.2.3"
end function get_libvers
