subroutine foo(a)
    real :: a(*, 10) ! error: star not in last dim
end subroutine