module allocate_13_module
    implicit none
    private

    public :: toml_value
    type :: toml_value
        integer, allocatable :: a(:)
    contains
        procedure :: value_proc
    end type toml_value
contains
    subroutine value_proc(self)
        class(toml_value), intent(inout) :: self
        allocate(self%a(2))
        deallocate(self%a)
    end subroutine value_proc
end module allocate_13_module

program allocate_13_program
    use allocate_13_module
    implicit none

    type(toml_value) :: tv
    allocate(tv%a(4))
    deallocate(tv%a)
    call tv%value_proc
    print *, "ok"
end program allocate_13_program
