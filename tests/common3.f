        subroutine pass()
            real A, B
            integer i(5)
            common /sample/ A, B, i
            print *, A, B
        end subroutine

        program common1
            real A, B
            integer c, d, e, f, g, h, i(5), j
            common /sample/ A, B, i
            common /c/ c /b/ d, e, f /c/ g, h, j
            A = 10
            B = 20
            call pass()
        end program

