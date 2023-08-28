module template_vector_m

    template vector_impl(T)
        type, deferred :: T
    end template

contains

    subroutine main()
    end subroutine

end module

program template_vector
    use template_vector_m
    implicit none
    call main()
end program