module derived_types_135_mod
    implicit none
    type :: inner_t
        integer :: dims(3)
    end type
    type :: outer_t
        type(inner_t) :: sub
    contains
        procedure :: work
    end type
contains
    pure subroutine work(this, arr, output)
        class(outer_t), intent(in) :: this
        real, dimension(:,:), intent(in) :: arr
        real, dimension(:,:), intent(out) :: output
        real, dimension(size(arr,1), this%sub%dims(1)) :: tmp
        tmp = 1.0
        output = tmp
    end subroutine
end module
program derived_types_135
    use derived_types_135_mod
    implicit none
    type(outer_t) :: obj
    real :: arr(3, 4), output(3, 4)
    obj%sub%dims = [4, 2, 1]
    arr = 2.0
    call obj%work(arr, output)
    if (any(output /= 1.0)) error stop

end program derived_types_135