subroutine assumed_size_nonlast_dim(a, b)
    integer, intent(in) :: a(*, 1)
    integer, intent(in) :: b
    print *, a(1,1), b
end subroutine