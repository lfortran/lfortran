program c_ptr_13
    use iso_c_binding, only: c_ptr, c_f_pointer, c_loc
    implicit none
    integer, target :: mydata(4,4)
    integer, pointer :: local_slice(:,:)
    type(c_ptr) :: p
    integer :: i, j

    mydata = reshape([(i, i=1,16)], [4,4])

    p = c_loc(mydata)
    call c_f_pointer(p, local_slice, shape(mydata))

    do j = 1, 4
        do i = 1, 4
            if (local_slice(i,j) /= mydata(i,j)) error stop "mismatch"
        end do
    end do
end program c_ptr_13
