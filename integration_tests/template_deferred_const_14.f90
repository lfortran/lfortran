module template_deferred_const_14_m
    implicit none
    template tmpl {n}
        deferred integer, parameter :: n
    end template
end module

program template_deferred_const_14
    use template_deferred_const_14_m
    implicit none
    print *, "ok"
end program
