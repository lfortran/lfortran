module class_27_mod
    implicit none
    type :: toml_value
        integer :: key
    end type toml_value

    type, extends(toml_value) :: toml_table
        logical :: implicit = .false.
    end type toml_table
end module class_27_mod

program class_27
    use class_27_mod
    implicit none

    class(toml_value), allocatable, target :: val
    type(toml_table), pointer :: tmp

    allocate(toml_table :: val)

    select type(val)
    type is(toml_table)
        val%key = 2
        tmp => val

    end select

    print *, "Key value: ", tmp%key
    if (tmp%key /= 2) error stop
end program
