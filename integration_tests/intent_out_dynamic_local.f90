program intent_out_dynamic_local
    use intent_out_dynamic_local_m, only: reset, check_local
    implicit none
    type :: program_item
        integer :: value = 11
        integer, allocatable :: data(:)
    end type
    type(program_item) :: x
    allocate(x%data(2))
    x%value = -1
    call reset(x)
    if (allocated(x%data) .or. x%value /= 11) error stop 5
    call check_local()
    block
        type :: block_item
            integer :: value = 31
            integer, allocatable :: data(:)
        end type
        type(block_item) :: y
        allocate(y%data(2))
        y%value = -1
        call reset(y)
        if (allocated(y%data) .or. y%value /= 31) error stop 6
    end block
end program
