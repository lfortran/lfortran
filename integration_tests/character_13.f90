program character_13
    ! Test character kind parameter with string literals
    integer, parameter :: tfc = selected_char_kind('DEFAULT')
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
end program character_13
