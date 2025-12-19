module m
    type, abstract :: Base
    contains
        procedure(work_interface), deferred :: work
    end type

    abstract interface
        subroutine work_interface(self)
            import Base
            class(Base), intent(inout) :: self
        end subroutine
    end interface

    type, extends(Base) :: Child
        integer :: value
    contains
        procedure :: work => child_work
    end type

contains

    subroutine child_work(self)
        class(Child), intent(inout) :: self
        print *, "Child work called, value =", self%value
    end subroutine

end module

program test_polymorphic_mold
    use m
    implicit none
    class(Base), allocatable :: src, obj

    ! Allocate src as Child type
    allocate(Child :: src)
    select type(src)
        type is (Child)
            src%value = 42
    end select

    ! Allocate obj with mold=src (should get Child type but not copy data)
    allocate(obj, mold=src)

    ! Set obj's value
    select type(obj)
        type is (Child)
            obj%value = 123
            if (obj%value /= 123) error stop "obj%value should be 123"
    end select

    ! Call deferred procedure
    call obj%work()

    print *, "Test passed"
end program
