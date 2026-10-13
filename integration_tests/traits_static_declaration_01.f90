! Compile-only trait syntax is an LFortran extension.
module traits_static_declaration_01_m
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
end module

program traits_static_declaration_01
    implicit none
end program
