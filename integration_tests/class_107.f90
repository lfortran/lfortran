module class_107_mod
    implicit none

    type, abstract :: abs_type
    contains
        procedure(alloc_iface), deferred :: alloc
    end type abs_type

    abstract interface
        subroutine alloc_iface(self, b, n)
            import :: abs_type
            class(abs_type), intent(in) :: self
            real(8), allocatable, intent(out) :: b(:,:)
            integer, intent(in) :: n
        end subroutine alloc_iface
    end interface

    type, extends(abs_type) :: my_type
    contains
        procedure :: alloc => my_alloc
    end type my_type

contains

    subroutine my_alloc(self, b, n)
        class(my_type), intent(in) :: self
        real(8), allocatable, intent(out) :: b(:,:)
        integer, intent(in) :: n
        allocate(b(2, n))
    end subroutine my_alloc

end module class_107_mod

program class_107
    use class_107_mod
    implicit none

    class(abs_type), allocatable :: objarr(:)

    allocate(my_type :: objarr(2))
    objarr = my_type()
    call check_allocator(objarr(1))

contains

    subroutine check_allocator(obj)
        class(abs_type), intent(in) :: obj
        real(8), allocatable :: b(:,:)

        call obj%alloc(b, 4)
        if (.not. allocated(b)) error stop
        if (any(shape(b) /= [2, 4])) error stop
        print *, "shape(b): ", shape(b)
    end subroutine check_allocator

end program class_107
