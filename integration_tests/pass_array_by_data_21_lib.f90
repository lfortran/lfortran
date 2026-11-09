subroutine pass_array_by_data_21_first(x, r)
    integer, intent(in) :: x(:)
    integer, intent(out) :: r
    r = x(1)
end subroutine

subroutine pass_array_by_data_21_add(x, s)
    integer, intent(inout) :: x(:)
    integer, intent(out) :: s
    x = x + 1
    s = sum(x)
end subroutine
