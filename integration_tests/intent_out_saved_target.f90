program intent_out_saved_target
    use intent_out_saved_target_module, only: check_saved, reset
    implicit none
    type :: record
        integer :: n = 41
    end type
    type(record), target, save :: data
    type :: program_item
        integer, pointer :: p => data%n
    end type
    call declaration_only()
    call check_saved(31)
    call check_saved(32)
    call check(23)
    call check(24)
    call local_record(43)
    call local_record(44)
    call local_bounds()
    call local_text('abc')
    call local_text('xyz')
    call local_block(47)
    call local_block(48)
    if (data%n /= 41) error stop 8
contains
    ! Even an unused local type must have a single-argument reset entry.
    subroutine declaration_only()
        integer, target, save :: target = 11
        type :: item
            integer, pointer :: p => target
        end type
        if (target /= 11) error stop 1
    end subroutine

    subroutine check(expected)
        integer, intent(in) :: expected
        integer, target, save :: target = 23
        type :: item
            integer, pointer :: p => target
        end type
        type(item), allocatable :: x
        if (target /= expected) error stop 2
        allocate(x)
        nullify(x%p)
        call reset(x)
        if (.not. associated(x%p, target)) error stop 3
        x%p = x%p + 1
        if (target /= expected + 1) error stop 4
        deallocate(x)
    end subroutine

    subroutine local_record(expected)
        integer, intent(in) :: expected
        type :: local_data
            integer :: n = 43
        end type
        type(local_data), target, save :: target
        type :: local_item
            integer, pointer :: p => target%n
        end type
        if (target%n /= expected) error stop 9
        target%n = target%n + 1
    end subroutine

    subroutine local_bounds()
        integer, parameter :: n = 3
        integer, target, save :: target(n) = [1, 2, 3]
        type :: array_item
            integer, pointer :: p(:) => target
        end type
        if (sum(target) /= 6) error stop 10
    end subroutine

    subroutine local_text(expected)
        character(*), intent(in) :: expected
        integer, parameter :: n = 3
        character(n), target, save :: target = 'abc'
        type :: text_item
            character(n), pointer :: p => target
        end type
        if (target /= expected) error stop 11
        target = 'xyz'
    end subroutine

    subroutine local_block(expected)
        integer, intent(in) :: expected
        saved_defaults: block
            type :: block_record
                integer :: n = 47
            end type
            type(block_record), target, save :: target
            type :: block_item
                integer, pointer :: p => target%n
            end type
            if (target%n /= expected) error stop 12
            target%n = target%n + 1
        end block saved_defaults
    end subroutine
end program
