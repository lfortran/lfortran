module legacy_array_sections_32_m
    implicit none
    type :: t
        integer :: a = 7
    end type
contains
    subroutine s(x)
        type(t), intent(out) :: x(1)
    end subroutine
end module

program legacy_array_sections_32
    use legacy_array_sections_32_m
    implicit none
    type(t) :: v(2)
    v%a = [1, 2]
    call s(v)
    print *, v%a
    if (v(1)%a /= 7) error stop
    if (v(2)%a /= 2) error stop
end program
