module bindc_optional_pointer_rank_01_m
    use iso_c_binding, only: c_int
    implicit none
    interface
        integer(c_int) function check(expected, x) bind(c, name="bindc_optional_pointer_rank_01_check")
            import :: c_int
            integer(c_int), value :: expected
            integer(c_int), pointer, optional, intent(inout) :: x(..)
        end function
    end interface
contains
    subroutine forward(expected, x)
        integer(c_int), intent(in) :: expected
        integer(c_int), pointer, optional, intent(inout) :: x(..)
        if (check(expected, x) /= 0_c_int) error stop 1
    end subroutine
end module

program bindc_optional_pointer_rank_01
    use bindc_optional_pointer_rank_01_m
    implicit none
    integer(c_int), target :: data(2), scalar, matrix(2, 2)
    integer(c_int), pointer :: p(:), ps, pm(:,:)

    data = [7_c_int, 8_c_int]
    scalar = 7_c_int
    matrix = reshape([7_c_int, 8_c_int, 9_c_int, 10_c_int], [2, 2])
    p => data
    ps => scalar
    pm => matrix
    call forward(0_c_int)
    call forward(1_c_int, p)
    call forward(3_c_int, ps)
    if (check(3_c_int, ps) /= 0_c_int) error stop 4
    call forward(4_c_int, pm)
    nullify(p)
    call forward(2_c_int, p)
    call forward(0_c_int)
    if (any(data /= [7_c_int, 8_c_int])) error stop 2
    if (scalar /= 7_c_int .or. any(matrix /= reshape( &
        [7_c_int, 8_c_int, 9_c_int, 10_c_int], [2, 2]))) error stop 3
end program
