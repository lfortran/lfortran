! Arrays, allocatables and pointers accessed through a namespace.
module namespace_modules_03_data
    implicit none
    integer :: fixed(5) = [1, 2, 3, 4, 5]
    real, allocatable :: dyn(:)
    integer, allocatable :: mat(:,:)
    integer, target :: tgt(3) = [10, 20, 30]
    integer, pointer :: ptr(:) => null()
    integer, pointer :: sptr => null()
end module

program namespace_modules_03
    use, namespace :: d => namespace_modules_03_data
    implicit none
    integer :: i

    ! Elements, sections and whole arrays
    if (d%fixed(3) /= 3) error stop
    if (any(d%fixed(2:4) /= [2, 3, 4])) error stop
    if (sum(d%fixed) /= 15) error stop
    if (size(d%fixed) /= 5) error stop
    if (lbound(d%fixed, 1) /= 1 .or. ubound(d%fixed, 1) /= 5) error stop
    d%fixed(1) = 100
    d%fixed(4:5) = 0
    d%fixed = d%fixed + 1
    if (any(d%fixed /= [101, 3, 4, 1, 1])) error stop

    ! Allocatable arrays
    if (allocated(d%dyn)) error stop
    allocate(d%dyn(4))
    if (.not. allocated(d%dyn)) error stop
    d%dyn = [(real(i), i = 1, 4)]
    if (abs(sum(d%dyn) - 10.0) > 1e-6) error stop
    deallocate(d%dyn)
    if (allocated(d%dyn)) error stop
    ! Reallocation on assignment
    d%dyn = [1.0, 2.0]
    if (size(d%dyn) /= 2) error stop

    allocate(d%mat(2, 3))
    d%mat = 7
    d%mat(2, 3) = 9
    if (sum(d%mat) /= 44) error stop
    if (any(shape(d%mat) /= [2, 3])) error stop

    ! Pointers: both sides of pointer assignment
    if (associated(d%ptr)) error stop
    d%ptr => d%tgt
    if (.not. associated(d%ptr, d%tgt)) error stop
    d%ptr(2) = 25
    if (d%tgt(2) /= 25) error stop
    d%sptr => d%tgt(3)
    d%sptr = 35
    if (d%tgt(3) /= 35) error stop
    nullify(d%ptr)
    if (associated(d%ptr)) error stop

    print *, d%fixed, d%tgt
end program
