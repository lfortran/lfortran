! The NOPASS and DEFERRED attributes of the bindings of a derived type
! declared in a module procedure.
module class_procedure_nopass_02_a
    implicit none
contains
    integer function get()
        get = 3
    end function

    subroutine set(r)
        integer, intent(out) :: r
        r = 5
    end subroutine
end module

module class_procedure_nopass_02_m
    use class_procedure_nopass_02_a, only: get, set
    implicit none
contains
    integer function h()
        type :: t
        contains
            procedure, nopass :: get
        end type
        type(t) :: a
        h = a%get()
    end function

    subroutine s(r)
        integer, intent(out) :: r
        call inner(r)
    contains
        subroutine inner(r)
            integer, intent(out) :: r
            type :: u
            contains
                procedure, nopass :: set
                procedure, nopass :: three => get
            end type
            type(u) :: b
            call b%set(r)
            r = r + b%three()
        end subroutine
    end subroutine

    subroutine abstract_nopass(r)
        integer, intent(out) :: r
        type, abstract :: a_t
        contains
            procedure(iget), deferred, nopass :: get
        end type
        abstract interface
            integer function iget()
            end function
        end interface
        interface
            integer function class_procedure_nopass_02_get()
            end function
        end interface
        type, extends(a_t) :: b_t
        contains
            procedure, nopass :: get => class_procedure_nopass_02_get
        end type
        type(b_t) :: b
        class(a_t), allocatable :: c
        allocate(b_t :: c)
        r = b%get() + c%get()
    end subroutine

    ! A deferred binding gets no procedure in the vtable of its abstract type.
    subroutine abstract_deferred(r)
        integer, intent(out) :: r
        type, abstract :: p_t
        contains
            procedure(pget), deferred :: get
        end type
        abstract interface
            integer function pget(self)
                import :: p_t
                class(p_t), intent(in) :: self
            end function
        end interface
        class(p_t), pointer :: x
        x => null()
        r = 6
        if (associated(x)) r = x%get()
    end subroutine
end module

integer function class_procedure_nopass_02_get()
    class_procedure_nopass_02_get = 4
end function

program class_procedure_nopass_02
    use class_procedure_nopass_02_m
    implicit none
    integer :: r
    if (h() /= 3) error stop 1
    call s(r)
    if (r /= 8) error stop 2
    call abstract_nopass(r)
    if (r /= 8) error stop 3
    call abstract_deferred(r)
    if (r /= 6) error stop 4
    print *, "ok"
end program
