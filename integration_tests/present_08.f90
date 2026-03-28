program present_08
    implicit none

    real(8), pointer :: aptr(:,:,:) => null()
    logical :: res

    call client(ptr=aptr, res=res)
    if (.not. res) error stop "Test 1 failed: null pointer should be present"

    call client(res=res)
    if (res) error stop "Test 2 failed: absent arg should not be present"

    print *, "All tests passed"

contains

    subroutine client(ptr, res)
        real(8), pointer, intent(out), optional :: ptr(:,:,:)
        logical, intent(out) :: res
        res = present(ptr)
    end subroutine client

end program present_08
