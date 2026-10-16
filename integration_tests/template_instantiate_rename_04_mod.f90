module template_instantiate_rename_04_m
    implicit none
    template t {T}
        deferred type :: T
        type :: outer
            type(T) :: v
        end type
    contains
        ! The dummy arguments and the function result have the local name of
        ! their renamed type.
        subroutine set(o, x)
            type(outer), intent(inout) :: o
            type(T), intent(in) :: x
            call inner(o)
        contains
            subroutine inner(o)
                type(outer), intent(inout) :: o
                o%v = x
            end subroutine
        end subroutine
        function get(o) result(r)
            type(outer), intent(in) :: o
            type(T) :: r
            associate (q => o%v)
                r = q
            end associate
        end function
        function mk(x) result(o)
            type(T), intent(in) :: x
            type(outer) :: o
            o%v = x
        end function
    end template
end module

module template_instantiate_rename_04_m2
    use template_instantiate_rename_04_m
    implicit none
    instantiate t {integer}, only: o => outer, set, get, mk
end module
