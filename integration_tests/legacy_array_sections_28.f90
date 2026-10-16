module legacy_array_sections_28_m
    implicit none
contains
    subroutine run(shape, expected)
        integer, intent(in) :: shape(:)
        real, intent(in) :: expected
        real :: a(8)
        integer :: i
        a = [(real(i), i = 1, 8)]
        if (shape_first(shape, a(2), 1) /= expected) error stop 1
        if (array_first(a(2), shape, 1) /= expected) error stop 2
    contains
        pure real function array_first(x, shape, dim) result(total)
            integer, intent(in) :: shape(:), dim
            real, intent(in) :: x(shape(1))
            integer :: k
            total = 0
            ! A variable DIM preserves the inquiry on the inlined descriptor.
            do k = 1, size(x, dim)
                total = total + x(k)
            end do
        end function array_first

        pure real function shape_first(shape, x, dim) result(total)
            integer, intent(in) :: shape(:), dim
            real, intent(in) :: x(shape(1))
            integer :: k
            total = 0
            do k = 1, size(x, dim)
                total = total + x(k)
            end do
        end function shape_first
    end subroutine run
end module legacy_array_sections_28_m

program legacy_array_sections_28
    use legacy_array_sections_28_m
    implicit none
    call run([3], 9.)
    call run([5], 20.)
end program legacy_array_sections_28
