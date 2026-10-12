! move_alloc between polymorphic allocatable arrays of different declared types
module move_alloc_class_02_mod
    implicit none

    type :: base_t
        integer :: a = 0
    contains
        procedure :: get => base_get
    end type

    type, extends(base_t) :: child_t
        integer :: b = 0
    contains
        procedure :: get => child_get
    end type

    type, extends(child_t) :: grandchild_t
        integer :: c = 0
    contains
        procedure :: get => grandchild_get
    end type

contains

    integer function base_get(self)
        class(base_t), intent(in) :: self
        base_get = -1
    end function

    integer function child_get(self)
        class(child_t), intent(in) :: self
        child_get = self%a + self%b
    end function

    integer function grandchild_get(self)
        class(grandchild_t), intent(in) :: self
        grandchild_get = self%a + self%b + self%c
    end function

    subroutine move(from, to)
        class(child_t), allocatable, intent(inout) :: from(:)
        class(base_t), allocatable, intent(inout) :: to(:)
        call move_alloc(from, to)
    end subroutine

end module

program move_alloc_class_02
    use move_alloc_class_02_mod
    implicit none
    class(base_t), allocatable :: t(:), t2(:), tm(:,:)
    class(child_t), allocatable :: s(:), m(:,:)
    class(*), allocatable :: u(:)
    integer :: i

    ! Unallocated source
    call move_alloc(s, t)
    if (allocated(t)) error stop 1

    allocate(s(3))
    s%b = 5
    call move_alloc(s, t)
    if (allocated(s)) error stop 2
    if (.not. allocated(t)) error stop 3
    if (size(t) /= 3) error stop 4
    select type (t)
    type is (child_t)
        if (any(t%b /= 5)) error stop 5
    class default
        error stop 6
    end select
    do i = 1, 3
        if (t(i)%get() /= 5) error stop 7
    end do

    ! Dynamic type differs from the source's declared type; lower bounds kept
    allocate(grandchild_t :: s(-1:1))
    select type (s)
    type is (grandchild_t)
        do i = -1, 1
            s(i)%a = i
            s(i)%b = 10
            s(i)%c = 100
        end do
    end select
    call move(s, t2)
    if (allocated(s)) error stop 8
    if (lbound(t2, 1) /= -1 .or. ubound(t2, 1) /= 1) error stop 9
    do i = -1, 1
        if (t2(i)%a /= i) error stop 10
        if (t2(i)%get() /= i + 110) error stop 11
    end do
    select type (t2)
    type is (grandchild_t)
        if (any(t2%c /= 100)) error stop 12
    class default
        error stop 13
    end select

    ! Rank 2
    allocate(m(2, 3))
    m%a = 1
    m%b = 2
    call move_alloc(m, tm)
    if (allocated(m)) error stop 14
    if (size(tm, 1) /= 2 .or. size(tm, 2) /= 3) error stop 15
    if (tm(2, 3)%get() /= 3) error stop 16

    ! Unlimited polymorphic destination
    allocate(s(4))
    s%b = 7
    call move_alloc(s, u)
    if (allocated(s)) error stop 17
    if (size(u) /= 4) error stop 18
    select type (u)
    type is (child_t)
        if (any(u%b /= 7)) error stop 19
    class default
        error stop 20
    end select

    print *, "ok"
end program
