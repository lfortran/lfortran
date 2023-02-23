module derived_type1

    implicit none
    integer, public, parameter :: tfc = selected_char_kind('DEFAULT')

    type enum_escape
        character(len=1, kind=tfc) :: newline = achar(10, kind=tfc)
    end type enum_escape

    type(enum_escape), public, parameter :: toml_escape = enum_escape()
    character(kind=tfc, len=*), public, parameter :: nl = &
        &' '//toml_escape%newline

end module derived_type1
