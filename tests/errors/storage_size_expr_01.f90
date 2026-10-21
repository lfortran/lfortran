program storage_size_expr_01
    integer :: a(2, 3)
    print *, storage_size(a + 1)
end program
