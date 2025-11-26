program character_14
    implicit none
    integer, parameter :: tfc = selected_char_kind('DEFAULT')
    character(1, tfc) :: hash = tfc_"a"
    print *, tfc_"a" == hash
    hash = 1_"a"

    if (len(hash) /= 1) error stop
    if (len(1_"a") /= 1) error stop
    if (hash /= 1_"a") error stop
end program character_14