! An `instantiate` inside the body of a separate module procedure written
! with the `module procedure` syntax must instantiate the bodies of the
! template's procedures, both for a deferred type and a deferred procedure.
module template_submodule_01_tmpl
    implicit none
    abstract interface
        function unop(x) result(r)
            integer, intent(in) :: x
            integer :: r
        end function
    end interface

    template ident_tmpl {a_t}
        deferred type :: a_t
    contains
        function ident(x) result(r)
            type(a_t), intent(in) :: x
            type(a_t) :: r
            r = x
        end function
    end template

    template apply_tmpl {p}
        deferred procedure (unop) :: p
    contains
        function apply_plus_one(x) result(r)
            integer, intent(in) :: x
            integer :: r
            r = p(x) + 1
        end function
    end template
end module

module template_submodule_01_ops
    implicit none
contains
    function dbl(x) result(r)
        integer, intent(in) :: x
        integer :: r
        r = 2*x
    end function
end module

module template_submodule_01_user
    implicit none
    interface
        module subroutine check_type()
        end subroutine

        module function check_proc(x) result(r)
            integer, intent(in) :: x
            integer :: r
        end function
    end interface
end module

submodule(template_submodule_01_user) template_submodule_01_user_s
    use template_submodule_01_tmpl
    use template_submodule_01_ops
    implicit none
contains
    module procedure check_type
        instantiate ident_tmpl {integer}
        print *, ident(3)
        if (ident(3) /= 3) error stop
    end procedure

    module procedure check_proc
        instantiate apply_tmpl {dbl}
        r = apply_plus_one(x)
    end procedure
end submodule

program template_submodule_01
    use template_submodule_01_user
    implicit none
    call check_type()
    print *, check_proc(3)
    if (check_proc(3) /= 7) error stop
end program
