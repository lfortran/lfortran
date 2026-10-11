module traits_runtime_inspection_result_oracle_m
    implicit none
    type :: Root
    end type
    type, extends(Root) :: Leaf
        integer :: n
    contains
        final :: finish
    end type
    integer :: calls = 0, finals = 0, total = 0
contains
    subroutine finish(value)
        type(Leaf), intent(inout) :: value
        finals = finals + 1
        total = total + value%n
        value%n = -777
    end subroutine
    function make() result(value)
        class(Root), allocatable :: value
        calls = calls + 1
        allocate(Leaf :: value)
        select type (value)
        type is (Leaf)
            value%n = 17
        end select
    end function
end module

program traits_runtime_inspection_03_oracle
    use traits_runtime_inspection_result_oracle_m
    implicit none
    select type (concrete => make())
    type is (Leaf)
        if (calls /= 1 .or. finals /= 0 .or. concrete%n /= 17) error stop 1
    class default
        error stop 2
    end select
    if (finals /= 1 .or. total /= 17) error stop 3
end program
