module traits_arrayitem_01_oracle_m
    implicit none
    type :: Box
        integer :: data
    contains
        procedure, pass(self) :: count => box_count
        procedure, pass(self) :: matrix => box_matrix
    end type
contains
    function box_count(extents, self, values) result(r)
        integer, intent(in) :: extents(1), values(extents(1))
        class(Box), intent(in) :: self
        integer :: r
        r = self%data + sum(values)
    end function

    function box_matrix(extents, indices, values, self) result(r)
        integer, intent(in) :: extents(2, 2), indices(2)
        integer, intent(in) :: values(extents(indices(1), 2), indices(2))
        class(Box), intent(in) :: self
        integer :: r
        r = self%data + sum(values)
    end function
end module

program traits_arrayitem_01_oracle
    use traits_arrayitem_01_oracle_m
    implicit none
    type(Box) :: x
    integer :: n(1), a(2), extents(2, 2), indices(2), matrix(3, 2)
    x = Box(10)
    n = [2]
    a = [1, 2]
    if (x%count(n, a) /= 13) error stop 1
    extents = reshape([1, 2, 3, 4], [2, 2])
    indices = [1, 2]
    matrix = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    if (x%matrix(extents, indices, matrix) /= 31) error stop 2
    extents(1, 2) = 2
    indices(2) = 1
    if (x%matrix(extents, indices, matrix(:2, :1)) /= 13) error stop 3
end program
