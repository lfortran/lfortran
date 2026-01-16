module derived_types_96_mod
    implicit none
    public :: string_t, operator(.in.)

    type :: string_t
        character(len=:), allocatable :: s
    end type string_t

    interface operator(.in.)
        module procedure string_in_array
    end interface

contains

    logical function string_in_array(lhs, rhs)
        character(len=*), intent(in) :: lhs
        type(string_t), intent(in) :: rhs(:)
        integer :: i

        string_in_array = .false.
        do i = 1, size(rhs)
                if (trim(lhs) == trim(rhs(i)%s)) then
                    string_in_array = .true.
                    return
                end if
        end do
    end function string_in_array

end module derived_types_96_mod

program derived_types_96
    use derived_types_96_mod, only: string_t, operator(.in.)
    implicit none

    type :: source_t
        type(string_t), allocatable :: modules_provided(:)
        type(string_t), allocatable :: modules_used(:)
    end type source_t

    type(source_t) :: source
    integer :: j
    logical :: check(3)
    check = .false.
    allocate(source%modules_provided(1))
    source%modules_provided(1)%s = "stdlib"
    allocate(source%modules_used(2))
    source%modules_used(1)%s = "stdlib"
    source%modules_used(2)%s = "iso_c_binding"

    do j = 1, size(source%modules_used)
        if (source%modules_used(j)%s .in. source%modules_provided) check(j) = .true.
        deallocate(source%modules_provided)
        allocate(source%modules_provided(0))
    end do

    if (check(1) .neqv. .true. .or. check(2) .neqv. .false. .or. check(3) .neqv. .false.) error stop "Test failed"
    check = .false.
    do j = 1, size(source%modules_used)
        if (source%modules_used(j)%s .in. source%modules_provided) check(j) = .true.
    end do
    if (check(1) .neqv. .false. .or. check(2) .neqv. .false. .or. check(3) .neqv. .false.) error stop "Test failed"
end program derived_types_96