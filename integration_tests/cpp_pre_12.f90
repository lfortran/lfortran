program cpp_pre_12
    implicit none
    call print_build_info()

contains
    subroutine print_build_info()
        !! Print the build information of the program.
        implicit none

        write(*,'("ATHENA: &
            &Adaptive Training for High Efficiency Neural network Applications")')
        write(*,'(" (build ",A,1X,A,")")') __DATE__, __TIME__
        
    end subroutine print_build_info
end program cpp_pre_12