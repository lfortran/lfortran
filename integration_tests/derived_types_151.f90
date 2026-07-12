module m_compute_151
    implicit none
contains
    subroutine compute(n)
        class(*), intent(in) :: n
        select type(n)
        type is (integer)
            if (n /= 42) error stop
        end select
    end subroutine
end module
module m_151
    implicit none
    type :: method
        integer :: nargs = 0
        procedure(), nopass, pointer :: f => null()
    end type
    interface method
        module procedure :: method_create
    end interface
contains
    function method_create(f, a1) result(that)
        procedure() :: f
        class(*), intent(in) :: a1
        type(method) :: that
        that%f => f
        that%nargs = 1
    end function
end module
program derived_types_151
    use m_151
    use m_compute_151
    implicit none
    integer, parameter :: i8 = selected_int_kind(18)
    type(method) :: mtd
    integer(i8) :: values(8)
    ! Test 1: procedure pointer in struct constructor
    mtd = method(compute, 42)
    if (mtd%nargs /= 1) error stop
    if (.not. associated(mtd%f)) error stop
    ! Test 2: date_and_time with integer(8) values array
    call date_and_time(values=values)
    if (values(1) < 2000 .or. values(1) > 2100) error stop
    if (values(2) < 1 .or. values(2) > 12) error stop
    if (values(3) < 1 .or. values(3) > 31) error stop
    print *, "PASS"
end program