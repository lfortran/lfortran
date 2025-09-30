module class_66_mod
    implicit none
    type, abstract :: bitset_type
        integer :: n = 0
    contains
        procedure(print_abstract), deferred, pass(self) :: print_base
        generic :: print => print_base
    end type bitset_type

    abstract interface
        subroutine print_abstract(self)
            import :: bitset_type
            class(bitset_type), intent(inout) :: self
        end subroutine print_abstract
    end interface

    type, extends(bitset_type) :: bitset_64
        integer :: extra = 64
    contains
        procedure, pass(self) :: print_base => print_bitset_64
    end type bitset_64

contains

    subroutine print_bitset_64(self)
        class(bitset_64), intent(inout) :: self
        self%n = 10
        self%extra = 20
    end subroutine print_bitset_64
end module class_66_mod

program class_66
    use class_66_mod
    class(bitset_64), allocatable :: b
    allocate(bitset_64 :: b)
    call b%print() 
    if (b%n /= 10 .or. b%extra /= 20) error stop
end program class_66
