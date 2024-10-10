module module_test_call_arg_structinstance_member
    implicit none
    type :: data_type
        real(4) :: arr(4)
    end type data_type

contains
    subroutine sub(arr)
        real(4), intent(inout) :: arr(:)
        integer :: i
        do i = 1, 4
            arr(i) = real(i, kind=4) + 10.0_4
        end do
    end subroutine sub
end module module_test_call_arg_structinstance_member

program test_call_arg_structinstance_member
    use module_test_call_arg_structinstance_member
    implicit none
    type(data_type) :: d
    d%arr = 0.0_4
    call sub(d%arr)

    print *, d%arr
    if (any(d%arr /= [11., 12., 13., 14.])) error stop
end program
