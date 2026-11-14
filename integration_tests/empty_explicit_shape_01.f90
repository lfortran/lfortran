subroutine empty_explicit_shape_01_s(a, n)
    integer :: n
    integer, dimension(2:n), intent(in) :: a
    if (a(2) > 0) then
    end if
end subroutine

program empty_explicit_shape_01
    integer :: a(0)
    call empty_explicit_shape_01_s(a, 1)
end program
