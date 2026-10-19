! The ordinary abstract-type counterpart of traits_runtime_conformance_query_03:
! a CLASS IS association borrows its selector, defining the original object.
module traits_runtime_conformance_query_03_oracle_m
    implicit none
    type, abstract :: ICounter
        integer :: n = 0
    contains
        procedure(get_interface), deferred :: get
        procedure(bump_interface), deferred :: bump
    end type
    abstract interface
        pure integer function get_interface(self)
            import :: ICounter
            class(ICounter), intent(in) :: self
        end function
        subroutine bump_interface(self, by)
            import :: ICounter
            class(ICounter), intent(inout) :: self
            integer, intent(in) :: by
        end subroutine
    end interface
    type, extends(ICounter) :: Cell
    contains
        procedure :: get => cell_get
        procedure :: bump => cell_bump
        final :: cell_final
    end type
    type :: Holder
        class(*), allocatable :: any
    end type
    integer :: finals = 0
contains
    pure integer function cell_get(self)
        class(Cell), intent(in) :: self
        cell_get = self%n
    end function
    subroutine cell_bump(self, by)
        class(Cell), intent(inout) :: self
        integer, intent(in) :: by
        self%n = self%n + by
    end subroutine
    subroutine cell_final(self)
        type(Cell), intent(inout) :: self
        finals = finals + 1
    end subroutine
    subroutine bump_any(object, by)
        class(*), intent(inout) :: object
        integer, intent(in) :: by
        select type (counter => object)
        class is (ICounter)
            call counter%bump(by)
        end select
    end subroutine
    subroutine bump_pointer(object, by)
        class(*), pointer, intent(in) :: object
        integer, intent(in) :: by
        select type (counter => object)
        class is (ICounter)
            call counter%bump(by)
        end select
    end subroutine
    integer function read_any(object) result(r)
        class(*), intent(in) :: object
        r = -1
        select type (counter => object)
        class is (ICounter)
            r = counter%get()
        end select
    end function
    subroutine reset_nested(object)
        class(*), intent(inout) :: object
        select type (counter => object)
        class is (ICounter)
            select type (concrete => counter)
            type is (Cell)
                concrete%n = 0
            end select
        end select
    end subroutine
    pure integer function pure_read(object) result(r)
        class(*), intent(in) :: object
        r = -1
        select type (counter => object)
        class is (ICounter)
            r = counter%get()
        end select
    end function
end module

program traits_runtime_conformance_query_03_oracle
    use traits_runtime_conformance_query_03_oracle_m
    implicit none
    type(Cell), target :: a
    class(*), pointer :: p
    type(Holder) :: h
    call bump_any(a, 1)
    if (a%n /= 1) error stop 1
    p => a
    call bump_pointer(p, 10)
    if (a%n /= 11 .or. read_any(p) /= 11 .or. pure_read(a) /= 11) error stop 2
    call reset_nested(a)
    if (a%n /= 0 .or. finals /= 0) error stop 3
    allocate(h%any, source=a)
    select type (counter => h%any)
    class is (ICounter)
        call counter%bump(3)
    end select
    if (read_any(h%any) /= 3 .or. a%n /= 0 .or. finals /= 0) error stop 4
    print *, "abstract class guards: borrowed definable and readonly associations"
end program
