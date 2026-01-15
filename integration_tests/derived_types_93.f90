module derived_types_93_mod
    implicit none

    type :: temp_test
        integer :: val
    end type temp_test 

    type :: dep_t
        character(len=:), allocatable :: proj_dir
        type(temp_test), allocatable :: tmp
    contains
        procedure :: equal_dep
        generic   :: operator(==) => equal_dep
    end type dep_t

contains

    logical function equal_dep(lhs, rhs)
        class(dep_t), intent(in) :: lhs, rhs
        
        if (allocated(lhs%proj_dir) .neqv. allocated(rhs%proj_dir)) then
            equal_dep = .false.
        else if (.not. allocated(lhs%proj_dir)) then
            equal_dep = .true.
        else
            equal_dep = lhs%proj_dir == rhs%proj_dir
        end if
    end function equal_dep

end module derived_types_93_mod

program derived_types_93
    use derived_types_93_mod

    type(dep_t), allocatable :: d1(:), d2(:)
    integer :: i

    allocate(d1(5), d2(5))

    do i = 1, 3
        d1(i)%proj_dir = "AAA"
        d2(i)%proj_dir = "AAA"
        allocate(d1(i)%tmp)
        allocate(d2(i)%tmp)
        d1(i)%tmp%val = i
        d2(i)%tmp%val = i
    end do

    do i = 1, size(d1)
        if(.not. d1(i) == d2(i)) error stop
    end do
end program derived_types_93