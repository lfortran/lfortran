module mod_array_06_transfer
    implicit none
    integer, allocatable :: val(:)

contains
    subroutine sub( value )
        character(*), intent(in) :: value
        allocate(val(len(value))) ! --realloc-lhs is not able to allocate val and so, adding explicit allocation
        val = transfer(value, val, 1 * len(value))
    end subroutine sub
end module

program array_06_transfer
    use mod_array_06_transfer
    character(4) :: value = "1234"
    call sub(value)
    print *, val ! Currently, output of transfer using String is not matching with gfortran
end program
