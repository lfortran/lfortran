module separate_compilation_11a_module
contains
subroutine parent_of_nested_subroutine(a, b)
    integer, intent(inout) :: a, b

    call nested_subroutine(a, b)

    contains
    subroutine nested_subroutine(c, d)
        integer, intent(inout) :: c, d
        c = c + 1
        d = d + 2
    end subroutine nested_subroutine
end subroutine parent_of_nested_subroutine

end module separate_compilation_11a_module
