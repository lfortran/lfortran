module mod_separate_compilation_32
    implicit none

    interface
        module function assemble_divergence(n) result(y)
            integer, intent(in) :: n
            real(8) :: y
        end function assemble_divergence
    end interface

end module mod_separate_compilation_32
