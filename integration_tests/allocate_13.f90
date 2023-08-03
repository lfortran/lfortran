module allocate_13_module
    implicit none
    private

    public :: toml_value

    type :: toml_value

    integer, allocatable :: a(:)

    end type toml_value


end module allocate_13_module

program allocate_13_program
    use allocate_13_module
    implicit none

    type(toml_value) :: tv
    allocate(tv%a(4))
    deallocate(tv%a)
    print *, "ok"
end program allocate_13_program
