! The provider: a Table of n entries, whose results include a type that only
! the provider declares.
module traits_runtime_polymorphic_result_separate_01_provider_m
    use traits_runtime_polymorphic_result_separate_01_contracts_m, only: ILookup, Point
    implicit none
    type, extends(Point) :: Point3
        integer :: z = 0
    end type Point3

    type :: Table
        integer :: n = 0
    end type Table
    implements ILookup :: Table
        procedure, pass :: lookup => table_lookup
        procedure, pass :: origin => table_origin
        procedure, pass :: size_of => table_size_of
    end implements
contains
    function table_lookup(self, key) result(res)
        class(Table), intent(in) :: self
        character(*), intent(in) :: key
        class(*), allocatable :: res
        select case (key)
        case ("n")
            res = self%n
        case ("half")
            res = self%n / 2d0
        case ("label")
            res = repeat("t", self%n)
        case default
            res = Point3(self%n, len(key), -1)
        end select
    end function table_lookup

    function table_origin(self) result(res)
        class(Table), intent(in) :: self
        class(Point), allocatable :: res
        if (self%n > 2) then
            res = Point3(1, 2, self%n)
        else
            res = Point(-1, -2)
        end if
    end function table_origin

    function table_size_of(self) result(res)
        class(Table), intent(in) :: self
        integer, allocatable :: res
        res = self%n
    end function table_size_of
end module traits_runtime_polymorphic_result_separate_01_provider_m
