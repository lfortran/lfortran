module template_struct_component_import_01_m
    implicit none
    abstract interface
        subroutine iface()
        end subroutine
    end interface
    type :: other
        integer :: a
    end type
    template t {T}
        deferred type :: T
        type :: holder
            procedure(iface), pointer, nopass :: f
            ! Components named like the interface and the type.
            real :: iface
            type(other) :: o
            integer :: other
            class(*), allocatable :: p
            type(T) :: v
        end type
    end template
    template outer {T}
        deferred type :: T
        template inner {U}
            deferred type :: U
            type :: nested_holder
                procedure(iface), pointer, nopass :: f
                type(other) :: o
                type(T) :: v
                type(U) :: w
            end type
        end template
    end template
contains
    subroutine hello()
    end subroutine
end module

program template_struct_component_import_01
    ! Neither `iface` nor `other` is visible here.
    use template_struct_component_import_01_m, only: t, outer, hello
    implicit none
    instantiate t {integer}
    instantiate outer {integer}, only: inner
    instantiate inner {real}, only: nested_holder
    type(holder) :: h
    type(nested_holder) :: n

    h%f => hello
    if (.not. associated(h%f)) error stop
    h%iface = 2.5
    h%o%a = 2
    h%other = 4
    h%v = 3
    if (abs(h%iface - 2.5) > 1e-6) error stop
    if (h%o%a /= 2) error stop
    if (h%other /= 4) error stop
    if (h%v /= 3) error stop
    allocate(h%p, source=5)
    select type (q => h%p)
    type is (integer)
        if (q /= 5) error stop
    class default
        error stop
    end select

    n%f => hello
    if (.not. associated(n%f)) error stop
    n%o%a = 6
    n%v = 7
    n%w = 1.5
    if (n%o%a /= 6) error stop
    if (n%v /= 7) error stop
    if (abs(n%w - 1.5) > 1e-6) error stop
    print *, "ok"
end program
