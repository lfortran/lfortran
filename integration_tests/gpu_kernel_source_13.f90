program gpu_kernel_source_13
    implicit none
    integer :: a(2,3), b(3,2), c(2,3), i, j

    do concurrent (j = 1:3, i = 1:2)
        a(i,j) = i + j
        b(j,i) = i - j
    end do
    do j = 1, 3
        do i = 1, 2
            if (a(i,j) /= i + j .or. b(j,i) /= i - j) error stop 1
        end do
    end do

    ! A writable scalar actual has the same conflicting layout as a direct store.
    do concurrent (j = 1:3, i = 1:2)
        a(i,j) = i + j
        call put_element(b(j,i), 2*i - j)
    end do
    do j = 1, 3
        do i = 1, 2
            if (a(i,j) /= i + j .or. b(j,i) /= 2*i - j) error stop 2
        end do
    end do

    ! Each call writes a disjoint element, but the actual hides its layout.
    do concurrent (j = 1:3, i = 1:2)
        a(i,j) = i + j
        call put_array(b, j, i, i - 2*j)
    end do
    do j = 1, 3
        do i = 1, 2
            if (a(i,j) /= i + j .or. b(j,i) /= i - 2*j) error stop 3
        end do
    end do

    ! Read-only array actuals must not conflict with the contiguous stores.
    do concurrent (j = 1:3, i = 1:2)
        a(i,j) = read_element(b, j, i)
        call copy_element(b, j, i, c(i,j))
    end do
    do j = 1, 3
        do i = 1, 2
            if (a(i,j) /= i - 2*j .or. c(i,j) /= i - 2*j) error stop 4
        end do
    end do

    do concurrent (j = 1:3, i = 1:2)
        call put_element(a(i,j), i + j)
    end do
    do j = 1, 3
        do i = 1, 2
            if (a(i,j) /= i + j) error stop 5
        end do
    end do
    print *, "ok"
contains
    pure subroutine put_element(destination, value)
        integer, intent(out) :: destination
        integer, intent(in) :: value
        destination = value
    end subroutine

    pure subroutine put_array(destination, row, column, value)
        integer, intent(inout) :: destination(:,:)
        integer, intent(in) :: row, column, value
        destination(row,column) = value
    end subroutine

    pure integer function read_element(source, row, column) result(value)
        integer, intent(in) :: source(:,:), row, column
        value = source(row,column)
    end function

    pure subroutine copy_element(source, row, column, value)
        integer, intent(in) :: source(:,:), row, column
        integer, intent(out) :: value
        value = source(row,column)
    end subroutine
end program
