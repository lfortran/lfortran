module template_deferred_interface_procedure_m
    implicit none
    template tt{f}
        deferred interface
            integer function f(y)
                integer, intent(in) :: y
            end function
            procedure :: f
        end interface
    end template
end module
