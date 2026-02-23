program separate_compilation_32
    use mod_separate_compilation_32, only: assemble_divergence
    implicit none

    if (assemble_divergence(1) /= 1.0d0) error stop 1
end program separate_compilation_32
