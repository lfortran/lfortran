module character_13_mod
    integer, parameter :: tfc = selected_char_kind('DEFAULT')
    character(kind=tfc, len=1) :: backslash = tfc_'\'
    character(kind=tfc, len=5) :: hello = tfc_"Hello"
end module

program character_13
    use character_13_mod
    ! Test character kind parameter with string literals
    type :: toml_char
        character(1, tfc) :: hash = tfc_"#"
        character(1, tfc) :: lbracket = tfc_"["
        character(1, tfc) :: rbracket = tfc_"]"
    end type
    
    type(toml_char) :: char_kind
    
    ! Test default initialization
    if (char_kind%hash /= "#") error stop
    if (char_kind%lbracket /= "[") error stop
    if (char_kind%rbracket /= "]") error stop
    
    ! Test explicit initialization
    char_kind = toml_char(tfc_"@", tfc_"(", tfc_")")
    if (char_kind%hash /= "@") error stop
    if (char_kind%lbracket /= "(") error stop
    if (char_kind%rbracket /= ")") error stop

    ! Test module level character variables
    if (backslash /= "\") error stop
    if (hello /= "Hello") error stop
end program character_13
