module bit_type_51
    implicit none

    type, abstract :: bitset_type
        integer :: flag
    contains
        procedure(from_string_abstract), deferred, pass(self) :: from_string 
    end type bitset_type

    type, extends(bitset_type) :: bitset_large
    contains
        procedure, pass(self)  :: from_string => from_string_large
    end type bitset_large

    abstract interface
        subroutine from_string_abstract(self)
            import :: bitset_type
            class(bitset_type), intent(out) :: self
        end subroutine from_string_abstract
    end interface 

   contains

    subroutine from_string_large(self)
        class(bitset_large), intent(out) :: self

        if (self%flag /= 1) error stop
    end subroutine from_string_large
end module

program derived_types_51
    use bit_type_51
    implicit none

    type(bitset_large) :: b
    b%flag = 1
    call b%from_string()
end program derived_types_51
