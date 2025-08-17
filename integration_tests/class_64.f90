module class_64_mod_1
    type, abstract :: toml_value
        integer :: x = 0
    contains
        procedure :: accept
    end type toml_value
contains
    subroutine accept(self)
        class(toml_value), intent(inout) :: self
        self%x = self%x + 1
    end subroutine accept
end module


module class_64_mod_2
    use class_64_mod_1
    type, extends(toml_value) :: toml_keyval
    end type toml_keyval
contains
subroutine temp(visitor, l1)
    class(toml_value), intent(inout), target :: visitor
    class(toml_value), pointer :: ptr
    logical :: l1
    ptr => visitor
    if (l1) then
        select type(ptr)
        class is(toml_keyval)
            call ptr%accept()
        end select
    end if

    select type(ptr)
    class is(toml_keyval)
        call ptr%accept()
    end select
end subroutine

end module class_64_mod_2

program class_64
    use class_64_mod_2
    type(toml_keyval) :: key
    call temp(key, .true.)
    if (key%x /= 2) error stop
    call temp(key, .false.)
    if (key%x /= 3) error stop
end program class_64
