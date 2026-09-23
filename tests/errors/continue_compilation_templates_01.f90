module continue_compilation_templates_01_mod
    implicit none

    ! Duplicate parameter name in requirement's namelist
    requirement dup_param_req {T, T, op_func}
        deferred type :: T
        deferred interface
            function op_func(x) result(y)
                type(T), intent(in) :: x
                type(T) :: y
            end function
        end interface
    end requirement

    ! A second, independent duplicate-parameter requirement, to verify
    ! compilation continues past the first error above and still
    ! reports this one too.
    requirement dup_param_req2 {V, V, W, comp_func}
        deferred type :: V
        deferred type :: W
        deferred interface
            function comp_func(x, y) result(z)
                type(V), intent(in) :: x
                type(V), intent(in) :: y
                logical :: z
            end function
        end interface
    end requirement

    ! A recoverable error inside a template body. The symbol table visitor adds
    ! the Template symbol only after the whole template is built, so letting the
    ! abort escape left the module without it and the body visitor then looked
    ! it up and asserted. Two errors, to show the template keeps being processed
    ! past the first one.
    ! C1603 restricts a template specification part to declarations with the
    ! PARAMETER attribute, so the two erroneous declarations below are named
    ! constants.
    template redecl_tmpl(T)
        deferred type :: T
        integer, parameter :: n = 1
        real, parameter :: n = 1.0
        integer, parameter :: bad = "abc"
    end template

    ! C1637: the interface-stmt of an interface block that is a
    ! requirement-specification shall specify ABSTRACT or DEFERRED. A plain
    ! interface block declares an external procedure with an explicit
    ! interface, which is not a deferred-argument declaration.
    requirement plain_interface_req {T2, plain_func}
        deferred type :: T2
        interface
            function plain_func(x) result(y)
                type(T2), intent(in) :: x
                type(T2) :: y
            end function
        end interface
    end requirement

    ! A generic interface block is rejected for the same reason: it builds a
    ! generic set out of procedures declared elsewhere instead of declaring a
    ! deferred argument. A second one, to show compilation continues.
    requirement operator_interface_req {U, plus_u}
        deferred type :: U
        interface operator (+)
            procedure plus_u
        end interface
        deferred interface
            function plus_u(x, y) result(z)
                type(U), intent(in) :: x, y
                type(U) :: z
            end function
        end interface
    end requirement

end module continue_compilation_templates_01_mod

program continue_compilation_templates_01
    use continue_compilation_templates_01_mod
    implicit none
end program continue_compilation_templates_01
