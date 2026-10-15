! An error in the header of a requirement or a template must not leave the
! rest of the module in the discarded requirement or template scope: the
! module procedures that follow are still defined and can be called (#13463).
module continue_compilation_templates_02_requirement
    implicit none
    requirement requirement_scope_r {t, t}  ! {Error} Parameter 't' is declared more than once in requirement 'requirement_scope_r'
        deferred type :: t
    end requirement
contains
    subroutine after_requirement_s()
    end subroutine
end module

module continue_compilation_templates_02_template
    implicit none
    template template_scope_t {t, u}  ! {Error} Template argument u has not been declared in template specification.
        deferred type :: t
    end template
contains
    subroutine after_template_s()
    end subroutine
end module

! The operator interfaces of the module declared before a rejected
! requirement or template are kept, and the template context ends with it.
module continue_compilation_templates_02_operators
    implicit none
    type :: v
        integer :: x
    end type
    interface operator(+)
        module procedure add_v
    end interface
    interface operator(-)
        module procedure sub_v
    end interface
    requirement requirement_op_r {t, u}  ! {Error} requirement argument 'u' has not been declared in requirement 'requirement_op_r'
        deferred type :: t
    end requirement
    template template_op_t {t, u}  ! {Error} Template argument u has not been declared in template specification.
        deferred type :: t
    end template
contains
    function add_v(a, b) result(c)
        type(v), intent(in) :: a, b
        type(v) :: c
        c%x = a%x + b%x
    end function
    function sub_v(a, b) result(c)
        type(v), intent(in) :: a, b
        type(v) :: c
        c%x = a%x - b%x
    end function
    subroutine after_template_deferred_s()
        deferred integer, parameter :: n  ! {Error} a `deferred` declaration is only allowed in a requirement, a template or a templated subprogram
    end subroutine
end module

! A templated function does not leave the template context set either.
module continue_compilation_templates_02_templated_function
    implicit none
contains
    template function templated_f{t}(x) result(y)
        deferred type :: t
        type(t), intent(in) :: x
        type(t) :: y
        y = x
    end function
    subroutine after_templated_function_s()
        deferred integer, parameter :: n  ! {Error} a `deferred` declaration is only allowed in a requirement, a template or a templated subprogram
    end subroutine
end module

! A template's deferred arguments are not procedure arguments after it.
module continue_compilation_templates_02_template_args
    implicit none
    template tt{n}
        deferred integer, parameter :: n
    end template
    integer, intent(in) :: n  ! {Error} Intent attribute can only be applied to procedure arguments
end module

program continue_compilation_templates_02
    use continue_compilation_templates_02_requirement
    use continue_compilation_templates_02_template
    use continue_compilation_templates_02_operators
    implicit none
    type(v) :: a, b
    call after_requirement_s()
    call after_template_s()
    a%x = 1
    b%x = 2
    a = a + b
    a = a - b
end program
