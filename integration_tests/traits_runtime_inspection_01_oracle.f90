module traits_runtime_inspection_oracle_m
    implicit none
    type :: Root
        integer :: n
    end type
    type, extends(Root) :: Mid
        integer :: middle
    end type
    type, extends(Mid) :: Leaf
        integer :: last
    end type
    type :: LeftTwin
        integer :: n
    end type
    type :: RightTwin
        integer :: n
    end type
contains
    subroutine mutate_target(view)
        class(Root), pointer, intent(in) :: view
        select type (concrete => view)
        type is (Leaf)
            concrete%n = concrete%n + 1
        class default
            error stop 1
        end select
    end subroutine
end module

program traits_runtime_inspection_01_oracle
    use traits_runtime_inspection_oracle_m
    implicit none
    type(Leaf), target :: first, second
    type(Leaf), pointer :: expected
    type(LeftTwin), target :: left
    type(RightTwin), target :: right
    class(Root), pointer :: view
    class(*), pointer :: twin
    integer :: selected, i
    first%n = 17
    second%n = 29
    expected => first
    view => first
    select type (concrete => view)
    type is (Leaf)
        if (.not. associated(expected, concrete)) error stop 2
        concrete%n = 23
        view => second
        if (concrete%n /= 23) error stop 3
        if (.not. associated(expected, concrete)) error stop 4
    class default
        error stop 5
    end select
    if (first%n /= 23 .or. view%n /= 29) error stop 6
    call mutate_target(view)
    if (second%n /= 30) error stop 7
    selected = 0
    select type (concrete => view)
    class is (Root)
        selected = 1
    class is (Mid)
        selected = 2
    end select
    if (selected /= 2) error stop 8
    selected = 0
    select type (concrete => view)
    class is (Root)
        selected = 1
    class is (Mid)
        selected = 2
    type is (Leaf)
        selected = 3
    end select
    if (selected /= 3) error stop 9
    select type (concrete => view)
    type is (Root)
        error stop 10
    class default
        if (concrete%n /= 30) error stop 11
    end select
    left%n = 41
    right%n = 41
    if (storage_size(left) /= storage_size(right)) error stop 12
    do i = 1, 2
        if (i == 1) then
            twin => left
        else
            twin => right
        end if
        select type (concrete => twin)
        type is (LeftTwin)
            if (i /= 1 .or. concrete%n /= 41) error stop 13
        type is (RightTwin)
            if (i /= 2 .or. concrete%n /= 41) error stop 14
        class default
            error stop 15
        end select
    end do
end program
