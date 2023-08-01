module module_52_tomlf_type
implicit none

type :: toml_table
    logical :: implicit = .false.
    logical :: inline = .false.
end type toml_table

end module
