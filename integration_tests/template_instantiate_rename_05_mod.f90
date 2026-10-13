module template_instantiate_rename_05_m
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
            o%v = x
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

! The instantiations are owned by module procedures, so they are loaded from
! this module's .mod file together with the procedures' bodies.
module template_instantiate_rename_05_m2
    use template_instantiate_rename_05_m
    implicit none
contains
    integer function run(x)
        integer, intent(in) :: x
        instantiate t {integer}, only: o => outer, set, get, mk
        type(o) :: a
        call set(a, x)
        if (a%v /= x) error stop
        a = mk(get(a) + 1)
        run = a%v
    end function

    real function run_internal(x)
        real, intent(in) :: x
        run_internal = helper(x)
    contains
        real function helper(y)
            real, intent(in) :: y
            instantiate t {real}, only: o => outer, set, get
            type(o) :: a
            call set(a, y)
            helper = get(a)
        end function
    end function
end module
