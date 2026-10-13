module template_instantiate_rename_03_m
    implicit none
    template t {T}
        deferred type :: T
        type :: inner
            type(T) :: v
        end type
        type :: outer
            type(T) :: v
            type(inner) :: i
        end type
    contains
        ! The dummy argument has the local name of its renamed type.
        subroutine set(o, x)
            type(outer), intent(inout) :: o
            type(T), intent(in) :: x
            o%v = x
            o%i%v = x
        end subroutine
    end template
end module

program template_instantiate_rename_03
    use template_instantiate_rename_03_m
    implicit none
    instantiate t {integer}, only: o => outer, set
    instantiate t {real}, only: o2 => outer, s2 => set
    type(o) :: a
    type(o2) :: b

    call set(a, 3)
    if (a%v /= 3) error stop
    if (a%i%v /= 3) error stop

    call s2(b, 2.5)
    if (abs(b%v - 2.5) > 1e-6) error stop
    if (abs(b%i%v - 2.5) > 1e-6) error stop
    print *, "ok"
end program
