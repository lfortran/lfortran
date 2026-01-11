module class_77_mod
    implicit none

    type :: toml_value
      integer, allocatable :: x(:)
    end type toml_value

    type, extends(toml_value) :: toml_table
      class(toml_value), allocatable :: table_key
    end type toml_table

end module class_77_mod


program class_77
    use class_77_mod
    implicit none
    type(toml_table), allocatable :: table
    class(toml_value), allocatable :: value
    allocate(table)
    allocate(toml_table :: table%table_key)
    allocate(table%table_key%x(5))
    table%table_key%x = [1,2,3,4,5]
    value = table

    select type(value)
      type is (toml_table)
        print *, allocated(value%table_key), value%table_key%x
        if (.not. allocated(value%table_key)) error stop
        if (any(value%table_key%x /= [1,2,3,4,5])) error stop
      class default 
        error stop
    end select
end program class_77
