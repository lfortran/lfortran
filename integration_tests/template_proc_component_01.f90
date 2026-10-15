! A `nopass` procedure-pointer component of a derived type declared in a
! template keeps its attributes when the template is instantiated.
module template_proc_component_01_m
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
            procedure(plus), pointer, nopass :: f
        end type
    contains
        function combine(h, a) result(r)
            type(holder), intent(in) :: h
            type(T), intent(in) :: a
            type(T) :: r
            r = h%f(a, a)
        end function
    end template
end module

program template_proc_component_01
    use template_proc_component_01_m, only: t
    implicit none
    instantiate t {integer, add}
    type(holder) :: h
    h%f => add
    if (h%f(1, 2) /= 3) error stop 1
    if (combine(h, 4) /= 8) error stop 2
    print *, "ok"
contains
    integer function add(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function
end program
