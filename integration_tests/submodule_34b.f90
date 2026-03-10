module submodule_34_string_mod
    implicit none
    type :: string_type
        character(:), allocatable :: raw
    end type
    interface len
        module procedure :: len_string
    end interface
    interface char
        module procedure :: char_string
    end interface
contains
    elemental function len_string(string) result(length)
        type(string_type), intent(in) :: string
        integer :: length
        if (allocated(string%raw)) then
            length = len(string%raw)
        else
            length = 0
        end if
    end function
    pure function char_string(string) result(res)
        type(string_type), intent(in) :: string
        character(len=len(string)) :: res
        res = ''
        if (allocated(string%raw)) then
            res = string%raw
        end if
    end function
end module submodule_34_string_mod

module submodule_34_parent_mod
    use submodule_34_string_mod, only: string_type
    implicit none
    interface
        module subroutine test_sub(x)
            integer, intent(out) :: x
        end subroutine
    end interface
end module submodule_34_parent_mod

submodule(submodule_34_parent_mod) submodule_34_impl
    use submodule_34_string_mod, only: string_type, char
contains
    module subroutine test_sub(x)
        integer, intent(out) :: x
        type(string_type) :: s
        character(:), allocatable :: t
        s%raw = "hello"
        t = char(s)
        x = len(t)
    end subroutine test_sub
end submodule submodule_34_impl
