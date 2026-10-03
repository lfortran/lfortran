submodule(mod_separate_compilation_32) submod_separate_compilation_32
contains

    module procedure assemble_divergence
        associate(dummy => n)
            block
                y = e(dummy)
            end block
        end associate
    contains
        function e(i) result(v)
            integer, intent(in) :: i
            real(8) :: v
            v = i
        end function e
    end procedure assemble_divergence

end submodule submod_separate_compilation_32
