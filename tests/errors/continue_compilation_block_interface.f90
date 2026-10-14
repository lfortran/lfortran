! A bound or a length in an interface body in a BLOCK construct can reference
! a variable of the BLOCK only as `n` or `n op m` in an upper bound or a
! length. Any other form is reported at the bound, instead of producing an
! invalid procedure type.
program continue_compilation_block_interface
    implicit none
    type :: t
        integer :: n
    end type t
    block
        type(t) :: v
        integer :: n
        integer, allocatable :: k
        abstract interface
            subroutine component_bound(a)
                import :: v
                integer, intent(inout) :: a(v%n)
            end subroutine component_bound
            subroutine lower_bound(a)
                import :: n
                integer, intent(inout) :: a(n:n+2)
            end subroutine lower_bound
            subroutine allocatable_bound(a)
                import :: k
                integer, intent(inout) :: a(k)
            end subroutine allocatable_bound
            function intrinsic_length() result(c)
                import :: n
                character(len=max(n, 1)) :: c
            end function intrinsic_length
        end interface
        v%n = 3
        n = 3
    end block
end program continue_compilation_block_interface
