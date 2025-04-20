module bit_type_52
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

    type, extends(bitset_large) :: mytype
    end type mytype

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

program derived_types_52
    use bit_type_52
    implicit none

    type(mytype) :: b
    b%flag = 1

    call b%from_string()
end program derived_types_52
