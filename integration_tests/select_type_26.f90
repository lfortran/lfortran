module select_type_26_mod
    implicit none

    type, abstract :: base_getter
    contains
        procedure(get_iface), deferred :: get
    end type base_getter

    abstract interface
        function get_iface(self, key) result(res)
            import
            class(base_getter), intent(in) :: self
            character(*), intent(in) :: key
            class(*), allocatable :: res
        end function get_iface
    end interface

    type, abstract :: base_type
    end type base_type

    type, extends(base_type) :: my_type
        integer :: val
    end type my_type

    type, extends(base_getter) :: my_getter
    contains
        procedure :: get => my_getter_get
    end type my_getter

contains

    function my_getter_get(self, key) result(res)
        class(my_getter), intent(in) :: self
        character(*), intent(in) :: key
        class(*), allocatable :: res
        if (key == "base_type") then
            allocate(res, source=my_type(42))
        end if
    end function my_getter_get

    subroutine test_select_type_func_call(g, val)
        class(base_getter), intent(in) :: g
        integer, intent(out) :: val
        val = 0
        select type(obj => g%get("base_type"))
        class is (base_type)
            select type(obj)
            type is (my_type)
                val = obj%val
            end select
        end select
    end subroutine test_select_type_func_call

end module select_type_26_mod

program select_type_26
    use select_type_26_mod
    implicit none

    type(my_getter) :: g
    integer :: val

    call test_select_type_func_call(g, val)
    print *, val
    if (val /= 42) error stop

end program select_type_26
