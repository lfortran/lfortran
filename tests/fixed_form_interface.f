        program fixed_form_interface
            implicit none

            interface
                subroutine my_subroutine(a)
                    integer, intent(in) :: a
                end subroutine my_subroutine

                function my_function(b) result(res)
                    integer, intent(in) :: b
                    integer :: res
                end function my_function
            end interface
        contains

        end program fixed_form_interface
