module template_instantiate_rename_06_m
    implicit none
    template t {T}
        deferred type :: T
        type :: outer
            type(T) :: v
        end type
    contains
        subroutine set(o, x)
            type(outer), intent(inout) :: o
            type(T), intent(in) :: x
            o%v = x
        end subroutine
        function get(o) result(r)
            type(outer), intent(in) :: o
            type(T) :: r
            associate (q => o%v)
                r = q
            end associate
        end function
    end template
end module
