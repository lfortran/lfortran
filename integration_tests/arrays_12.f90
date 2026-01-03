program arrays_12
    implicit none

    integer :: i
    real, dimension(2, 2) :: B
    integer, parameter:: C(3, 2, 3) = reshape([(i, i = 1, 18)], [3, 2, 3])

    ! vector subscripts
    integer, dimension(3) :: U = [1, 3, 2]
    integer, dimension(4) :: V = [3, 1, 1, 2]

    ! test back to back reshape - C is 3 x 2 x 3 function
    B = reshape(reshape(C, [4]), [2, 2])

    print *, B

    print *, C(1, :, :)
    print *, C(2, :, :)
    print *, C(3, :, :)

    ! test vector as subscripts for rank 3 array
    print *, C(U, 2, V)
    print *, C(3, U(3:), V)

    ! test spread intrinsic
    print *, spread(C, 2, 2)
end program 

