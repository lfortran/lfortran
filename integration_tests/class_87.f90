module class_87_mod
    implicit none

    type, abstract :: toml_value
    end type toml_value

    type :: xx
        integer :: v = 0
    end type xx

    type, extends(toml_value) :: toml_table
        class(xx), allocatable :: data
    end type toml_table

contains

    function cast_to_table(ptr) result(table)
        class(toml_value), intent(in), target :: ptr
        type(toml_table), pointer :: table

        nullify(table)
        select type (ptr)
        type is (toml_table)
            table => ptr
        end select
    end function cast_to_table

end module class_87_mod


program class_87
    use class_87_mod
    implicit none

    class(toml_value), allocatable :: t
    type(toml_table)     :: p

    ! Allocate polymorphic object
    allocate(toml_table :: t)

    ! Allocate component
    select type (t)
    type is (toml_table)
        allocate(t%data)
        t%data%v = 42
    class default
        error stop "Wrong dynamic type"
    end select

    p = cast_to_table(t)

    if (.not. allocated(p%data)) then
        error stop "allocatable member not allocated"
    end if

    if (p%data%v /= 42) then
        error stop "data value corrupted"
    end if

    print *, "TEST PASSED"

end program class_87
