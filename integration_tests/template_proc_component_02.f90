! A procedure-pointer component with a `=> null()` initializer, in a derived
! type declared in a template, is instantiated with its initializer.
module template_proc_component_02_m
    implicit none
    template t {T, plus}
        deferred type :: T
        deferred interface
            function plus(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
        type :: holder
            procedure(plus), pointer, nopass :: f => null()
        end type
    contains
        logical function is_set(h)
            type(holder), intent(in) :: h
            is_set = associated(h%f)
        end function
    end template
end module

program template_proc_component_02
    use template_proc_component_02_m, only: t
    implicit none
    instantiate t {integer, add}
    type(holder) :: h
    if (associated(h%f)) error stop 1
    if (is_set(h)) error stop 2
    h%f => add
    if (.not. is_set(h)) error stop 3
    if (h%f(1, 2) /= 3) error stop 4
    print *, "ok"
contains
    integer function add(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function
end program
