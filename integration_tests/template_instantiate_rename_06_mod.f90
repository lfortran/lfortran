! The template comes from the .mod file of another module, so its procedures
! are instantiated while the module procedures below are still being declared.
module template_instantiate_rename_06_m2
    use template_instantiate_rename_06_m
    implicit none
contains
    integer function run(x)
        integer, intent(in) :: x
        instantiate t {integer}, only: o => outer, set, get
        type(o) :: a
        call set(a, x)
        run = get(a) + a%v
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
