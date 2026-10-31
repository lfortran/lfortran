program bindc_cfi_alloc_copyback_04
    implicit none
    interface
        subroutine c_alloc(res) bind(c)
            character(len=:), allocatable, intent(inout) :: res(:)
        end subroutine
        subroutine c_dealloc(res) bind(c)
            character(len=:), allocatable, intent(inout) :: res(:)
        end subroutine
    end interface
    character(len=:), allocatable :: r(:)

    call c_alloc(r)
    if (.not. allocated(r)) error stop 1
    if (size(r) /= 3) error stop 2
    if (len(r) /= 4) error stop 3
    if (r(1) /= "abcd" .or. r(2) /= "efgh" .or. r(3) /= "ijkl") error stop 4

    call c_dealloc(r)
    if (allocated(r)) error stop 5
    print *, "ok"
end program
