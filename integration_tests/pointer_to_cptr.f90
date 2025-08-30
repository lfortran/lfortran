program pointer_to_cptr
    use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_int
    implicit none

    integer, target :: arr(5) = [1, 2, 3, 4, 5]
    type(c_ptr) :: arr_ptr

    arr_ptr = c_loc(arr)

    call process_array(arr_ptr, size(arr))

contains

    subroutine process_array(ptr, n)
        use iso_c_binding, only: c_ptr, c_f_pointer, c_int
        type(c_ptr), intent(in) :: ptr
        integer, intent(in) :: n
        integer(c_int), pointer :: f_arr(:)
        integer :: i

        call c_f_pointer(ptr, f_arr, [n])

        print *, "Array elements from process_array:"
        do i = 1, n
            print *, "Element", i, "=", f_arr(i)
        end do
    end subroutine process_array

end program pointer_to_cptr
