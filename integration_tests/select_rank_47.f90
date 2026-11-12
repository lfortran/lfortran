! A select rank selector, or an array constructor of one, passed to a bind(c)
! type(*) buf(*) dummy is the address of the data, not an array descriptor.
module select_rank_47_mod
    use iso_c_binding, only: c_int, c_int64_t
    implicit none
    interface
        function sum_any(buf, n) result(r) bind(c, name='select_rank_47_sum')
            import :: c_int, c_int64_t
            type(*), intent(in) :: buf(*)
            integer(c_int), value :: n
            integer(c_int64_t) :: r
        end function
        function sum_i64(buf, n) result(r) bind(c, name='select_rank_47_sum')
            import :: c_int, c_int64_t
            integer(c_int64_t), intent(in) :: buf(*)
            integer(c_int), value :: n
            integer(c_int64_t) :: r
        end function
    end interface
contains
    function put(value) result(r)
        integer(c_int64_t), intent(in) :: value(..)
        integer(c_int64_t) :: r
        select rank (value)
        rank (0)
            r = sum_any([value], 1)
            if (r /= sum_i64([value], 1)) error stop 1
        rank (1)
            r = sum_any(value, size(value))
            if (r /= sum_i64(value, size(value))) error stop 2
        rank (2)
            r = sum_any(value, size(value))
            if (r /= sum_i64(value, size(value))) error stop 3
        rank default
            error stop 4
        end select
    end function
end module

program select_rank_47
    use select_rank_47_mod
    implicit none
    integer(c_int64_t) :: a(3), b(2, 2)
    a = [1_c_int64_t, 2_c_int64_t, 3_c_int64_t]
    b = reshape([1_c_int64_t, 2_c_int64_t, 3_c_int64_t, 4_c_int64_t], [2, 2])
    if (sum_any(a, 3) /= 123) error stop 5
    if (put(5_c_int64_t) /= 5) error stop 6
    if (put([6_c_int64_t, 7_c_int64_t]) /= 67) error stop 7
    if (put(b) /= 1234) error stop 8
end program
