        program fixed_form_interface
            implicit none
            integer :: i, arr(5)
            integer :: OUTPUT_UNIT

c           tests that 'interface' is tokenized correctly
            interface
                subroutine my_subroutine(a)
                    integer, intent(in) :: a
                end subroutine my_subroutine

                function my_function(b) result(res)
                    integer, intent(in) :: b
                    integer :: res
                end function my_function
            end interface

c           tests that 'allocate' is tokenized correctly
            integer :: num = 2
            integer, dimension(:), allocatable :: factors
            allocate ( factors(num) )
            factors = [3, 4]
c           tests that 'deallocate' is tokenized correctly
            deallocate(factors)

    1       FORMAT (TR1, A)
            print 1, factors

            do concurrent (i=1:10:2)
                arr(i) = i
            enddo

c           ensures that 'FLUSH' is tokenized
            FLUSH(OUTPUT_UNIT)
        contains

        end program fixed_form_interface
