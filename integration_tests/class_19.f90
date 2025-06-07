module toml_mod_19_class_19
    type :: toml_value
    contains 
        procedure :: destroy
    end type
    type :: toml_node
      class(toml_value), allocatable :: val
    contains 
      procedure :: cleanup
    end type toml_node
contains
    subroutine destroy(self, x)
        class(toml_value), intent(inout) :: self
        integer, intent(out) :: x
        x = x + 1
    end subroutine
    subroutine cleanup(self, x)
        class(toml_node), intent(inout) :: self
        integer, intent(out) :: x
        call self%val%destroy(x)
    end subroutine
end module

program class_19
    use toml_mod_19_class_19
    type(toml_node) :: tmp
    integer :: x = 0
    call tmp%val%destroy(x)
    ! if (x /= 1) error stop    !! TODO
    call tmp%cleanup(x)
    ! if (x /= 2) error stop    !! TODO
end program