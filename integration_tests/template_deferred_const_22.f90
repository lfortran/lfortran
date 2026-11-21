module template_deferred_const_22_m
    implicit none
    template tmpl {n}
        deferred integer, parameter :: n
    end template
end module

program template_deferred_const_22
    use template_deferred_const_22_m
    implicit none
    print *, "ok"
end program
