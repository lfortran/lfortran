module operator_overloading_05_module3
use operator_overloading_05_module2, only: string_type, operator(>=), assignment(=)
contains
pure subroutine insert_head( array )

        type(string_type), intent(inout) :: array(0:)

        type(string_type) :: tmp
        integer(4) :: i

        ! tmp = array(0)
        find_hole: do i=1, size(array, kind=4)-1
            ! if ( array(i) >= tmp ) exit find_hole
            ! array(i-1) = array(i)
        end do find_hole
        ! array(i-1) = tmp

end subroutine insert_head
end module

program main
end program
