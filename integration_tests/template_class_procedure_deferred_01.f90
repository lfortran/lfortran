! A deferred binding of a type declared in a template stays deferred in the
! instantiation, so its vtable gets no procedure for it.
module template_class_procedure_deferred_01_m
    implicit none
    template tm {t}
        deferred type :: t
        type, abstract :: a_t
        contains
            procedure(iget), deferred :: get
        end type
        abstract interface
            function iget(self) result(r)
                import :: a_t, t
                class(a_t), intent(in) :: self
                type(t) :: r
            end function
        end interface
    end template
end module

module template_class_procedure_deferred_01_u
    use template_class_procedure_deferred_01_m
    implicit none
    instantiate tm {integer}
    type, extends(a_t) :: c_t
        integer :: v = 0
    contains
        procedure :: get => cget
    end type
contains
    function cget(self) result(r)
        class(c_t), intent(in) :: self
        integer :: r
        r = self%v
    end function
end module

program template_class_procedure_deferred_01
    use template_class_procedure_deferred_01_u
    implicit none
    class(a_t), allocatable :: x
    allocate(c_t :: x)
    select type (x)
    type is (c_t)
        x%v = 7
    end select
    if (x%get() /= 7) error stop
    print *, "ok"
end program
