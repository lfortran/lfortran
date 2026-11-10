program arrays_reshape_56
    ! reshape whose shape argument is itself a reshape() call (#4649)
    implicit none
    complex(8) :: my_complex(2, 3)
    complex(8) :: expected(6)
    integer(4) :: shp(2) = reshape([2, 3], [2])
    integer :: i, j

    expected = [(1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12)]

    my_complex = reshape([(1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12)], reshape([2, 3], [2]))
    print *, my_complex
    do j = 1, 3
        do i = 1, 2
            if (abs(my_complex(i, j) - expected(i + 2*(j - 1))) > 1.0d-12) error stop
        end do
    end do

    my_complex = (0.0d0, 0.0d0)
    my_complex = reshape([(1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12)], shp)
    print *, my_complex
    do j = 1, 3
        do i = 1, 2
            if (abs(my_complex(i, j) - expected(i + 2*(j - 1))) > 1.0d-12) error stop
        end do
    end do
    if (abs(my_complex(2, 3) - (11.0d0, 12.0d0)) > 1.0d-12) error stop
end program
