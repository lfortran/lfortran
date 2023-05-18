        subroutine sub()
            real a, b, c
            common / block_1 / a, c, b
        end subroutine

        program main
            real a, b, c
            common / block_1 / a, b, c
        end program
