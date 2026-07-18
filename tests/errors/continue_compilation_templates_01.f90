module continue_compilation_templates_01_mod
    implicit none

    ! Duplicate parameter name in requirement's namelist
    requirement dup_param_req(T, T, op_func)
        type, deferred :: T
        interface
            function op_func(x) result(y)
                type(T), intent(in) :: x
                type(T) :: y
            end function
        end interface
    end requirement

    ! A second, independent duplicate-parameter requirement, to verify
    ! compilation continues past the first error above and still
    ! reports this one too.
    requirement dup_param_req2(V, V, W, comp_func)
        type, deferred :: V
        type, deferred :: W
        interface
            function comp_func(x, y) result(z)
                type(V), intent(in) :: x
                type(V), intent(in) :: y
                logical :: z
            end function
        end interface
    end requirement

end module continue_compilation_templates_01_mod

program continue_compilation_templates_01
    use continue_compilation_templates_01_mod
    implicit none
end program continue_compilation_templates_01
