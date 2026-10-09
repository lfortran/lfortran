module traits_runtime_inspection_io_01_oracle_m
    implicit none
    type :: Box
        integer :: n, unit
        logical :: opened
        character(80) :: filename
    end type
contains
    subroutine readonly(view)
        class(*), intent(in) :: view
        integer :: length
        logical :: exists, opened
        select type (concrete => view)
        type is (Box)
            inquire(iolength=length) concrete%n
            if (length /= storage_size(7) / 8) error stop 1
            inquire(unit=concrete%unit, opened=opened)
            if (.not. opened) error stop 2
            inquire(file=concrete%filename, exist=exists)
            if (concrete%n /= 47) error stop 3
        class default
            error stop 4
        end select
    end subroutine
    subroutine writable(view)
        class(*), pointer, intent(in) :: view
        select type (concrete => view)
        type is (Box)
            inquire(iolength=concrete%n) 7
            associate (opened => concrete%opened)
                inquire(unit=concrete%unit, opened=opened)
            end associate
        class default
            error stop 5
        end select
    end subroutine
end module

program traits_runtime_inspection_io_01_oracle
    use traits_runtime_inspection_io_01_oracle_m
    implicit none
    type(Box), target :: object
    class(*), pointer :: view
    class(*), allocatable :: owner
    object%n = 47
    object%filename = "traits_runtime_inspection_io_01.absent"
    open(newunit=object%unit, status="scratch")
    call readonly(object)
    if (object%n /= 47) error stop 6
    view => object
    call writable(view)
    if (object%n /= storage_size(7) / 8 .or. .not. object%opened) error stop 7
    if (.not. associated(view)) error stop 8
    allocate(owner, source=object)
    select type (concrete => owner)
    type is (Box)
        inquire(iolength=concrete%n) 7_8
        if (concrete%n /= storage_size(7_8) / 8) error stop 9
    end select
    if (object%n /= storage_size(7) / 8) error stop 10
    close(object%unit)
    nullify(view)
    deallocate(owner)
end program
