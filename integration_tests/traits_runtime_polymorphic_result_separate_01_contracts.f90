! The trait of traits_runtime_polymorphic_result_separate_01: its messages
! return an unlimited polymorphic object selected by a character argument, a
! polymorphic object of a type that the provider extends, and an allocatable
! integer.
module traits_runtime_polymorphic_result_separate_01_contracts_m
    implicit none
    type :: Point
        integer :: x = 0, y = 0
    end type Point

    abstract interface :: ILookup
        function lookup(self, key) result(res)
            type(itself), pass, intent(in) :: self
            character(*), intent(in) :: key
            class(*), allocatable :: res
        end function lookup
        function origin(self) result(res)
            import :: Point
            type(itself), pass, intent(in) :: self
            class(Point), allocatable :: res
        end function origin
        function size_of(self) result(res)
            type(itself), pass, intent(in) :: self
            integer, allocatable :: res
        end function size_of
    end interface
end module traits_runtime_polymorphic_result_separate_01_contracts_m
