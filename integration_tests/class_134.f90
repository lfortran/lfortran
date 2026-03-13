module class_134_mod
    implicit none
    type :: container
        class(*), allocatable :: value(:)
    end type
contains
    subroutine set_value(this, value)
        class(container), intent(out) :: this
        class(*), intent(in) :: value(:)
        this%value = value
    end subroutine
end module

program class_134
    use class_134_mod
    implicit none
    type(container) :: x
    call set_value(x, ['foo', 'bar'])
    if (.not. allocated(x%value)) error stop
    select type (v => x%value)
    type is (character(*))
        if (size(v) /= 2) error stop
        if (v(1) /= 'foo') error stop
        if (v(2) /= 'bar') error stop
    class default
        error stop
    end select
    print *, "PASS"
end program
