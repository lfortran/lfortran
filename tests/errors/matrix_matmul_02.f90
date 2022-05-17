program matrix_matmul_02
    implicit none
    character :: a(3, 3)
    integer :: b(3, 3)
    print *, matmul(b, a)
end program
