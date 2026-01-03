module constructor_mod
    implicit none
    type :: toml_map
    end type toml_map

    type :: toml_table
        class(toml_map), allocatable :: map
    end type toml_table

    interface toml_table
        module procedure toml_table_init
    end interface toml_table
contains
    function toml_table_init() result(this)
        type(toml_table) :: this
        allocate(toml_map :: this%map)
    end function toml_table_init
end module constructor_mod

module reexport_mod
    use constructor_mod, only: toml_table
end module reexport_mod

program main
    use reexport_mod, only: toml_table
    implicit none
    type(toml_table) :: tabx
    tabx = toml_table()
    if (.not. allocated(tabx%map)) error stop 1
end program main
