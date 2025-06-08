program matrix_matmul_04
    implicit none
    integer :: a(3, 3, 3)
    integer :: b(3, 3)
    print *, matmul(a, b)
end program
