        module implicit_typing_09_mthdef
cx
                integer(kind=4), parameter :: isngl=4
                integer(kind=4), parameter :: idble=8
                integer(kind=4), parameter :: rsngl=4
                integer(kind=4), parameter :: rdble=8
                integer(kind=4), parameter :: rquad=16
c
                integer(kind=4), parameter :: iknd=isngl
                integer(kind=4), parameter :: rknd=rdble
cy
        end module

        program implicit_typing_09
                use implicit_typing_09_mthdef
cx
                implicit real(kind=rsngl) (a-c)
                implicit integer(kind=iknd) (i-k)
                implicit real(kind=4) (l-n)
                
                a = 1.323459_rsngl
                b = 2.0_rsngl
                c = a + b
                
                i = 5
                j = 10
                k = i + j
                
                l = 1.0
                m = 3.0
                n = l + m
                                
                if (abs(c - 3.323459_rsngl) > 1.0e-6) error stop 1
                if (k /= 15) error stop 2

                if (abs(n - 4.0) > 1.0e-6) error stop 3

                call test_implicit_real_subroutine()
                print *, test_implicit_complex()
            
            contains

                subroutine test_implicit_real_subroutine()
                use implicit_typing_09_mthdef
                implicit real(kind=idble) (q-s)
                
                q = 1.23456789d0
                r = 9.87654321d0
                s = q + r
                
                if (abs(s - 11.11111110d0) > 1.0d-6) error stop 4
                
                end subroutine

                integer function test_implicit_complex()
                use implicit_typing_09_mthdef
                implicit complex(kind=rdble) (t-v)
                
                t = cmplx(1.0, 2.0)
                u = cmplx(3.0, 4.0)
                v = t + u

                if (abs(real(v) - 4.0) > 1.0d-6) error stop 5
                if (abs(aimag(v) - 6.0) > 1.0d-6) error stop 6

                test_implicit_complex = 1
                
                end function

        end program
