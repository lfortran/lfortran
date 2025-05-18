module toml_mod_19_class_19
    type :: toml_value
    contains 
        procedure :: destroy
    end type
    type :: toml_node
      class(toml_value), allocatable :: val
    end type toml_node
contains
    subroutine destroy(self)
        class(toml_value), intent(inout) :: self
    end subroutine
end module

program class_19
    use toml_mod_19_class_19
    type(toml_node) :: tmp
    call tmp%val%destroy
end program