        program fixed_form_interface
            implicit none

c           tests that 'interface' is tokinized correctly
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
        contains

        end program fixed_form_interface
