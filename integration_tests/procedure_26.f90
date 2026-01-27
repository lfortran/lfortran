module procedure_26_mod_1
    implicit none
    private
    public :: error_type

    type :: error_type
        integer :: stat = 0
        character(len=:), allocatable :: message
    end type error_type

end module procedure_26_mod_1

module procedure_26_mod_2
    use procedure_26_mod_1, only: error_type
    implicit none
    private
    public :: check

    interface check
        module procedure :: check_stat
        module procedure :: check_logical
    end interface check

contains

    subroutine check_stat(error, stat, message)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: stat
        character(len=*), intent(in), optional :: message

        if (stat /= 0) then
            allocate(error)
            error%stat = stat
            if (present(message)) error%message = message
        end if
    end subroutine check_stat

    subroutine check_logical(error, expression, message)
        type(error_type), allocatable, intent(out) :: error
        logical, intent(in) :: expression
        character(len=*), intent(in), optional :: message

        if (.not. expression) then
            allocate(error)
            error%stat = 1
            if (present(message)) error%message = message
        end if
    end subroutine check_logical

end module procedure_26_mod_2

program procedure_26
    use procedure_26_mod_1, only: error_type
    use procedure_26_mod_2, only: check
    implicit none

    type(error_type), allocatable :: error
    integer, pointer :: ptr => null()

    call check(error, .not.associated(ptr), "Pointer should not be associated")

    if (allocated(error)) then
        error stop "Test failed: "//trim(error%message)
    else
        print *, "Test passed"
    end if

end program procedure_26
