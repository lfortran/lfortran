module template_array_05_m

    implicit none
    private
    public :: vector_t, matrix_t

    requirement op(t, plus_t)
        type, deferred :: t
        pure elemental function plus_t(l, r) result(rs)
            type(t), intent(in) :: l, r
            type(t) :: rs
        end function
    end requirement

    template vector_t(t, plus_t, n)
        require :: op(t, plus_t)
        integer :: n
        
        private
        public :: add_array

        type :: vector
            type(t) :: elements(n)
        end type
    contains
        pure function add_vector(a, b) result(r)
            type(vector), intent(in) :: a, b
            type(vector) :: r
            r%elements = plus_t(a%elements, b%elements)
        end function
    end template

    template matrix_t(t, plus_t, n)
        require :: op(t, plus_t)
        integer :: n
        
        private
        public :: add_matrix

        type :: matrix
            type(t) :: elements(n, n)
        end type
    contains
        pure function add_matrix(a, b) result(r)
            type(matrix), intent(in) :: a, b
            type(matrix) :: r
            r%elements = plus_t(a%elements, b%elements)
        end function
    end template

end module

program template_array_05

use template_array_05_m

integer, parameter :: n = 10
instantiate vector_t(integer, operator(+), n), &
    only: int_vector => vector, &
          int_add_vector => add_vector

type(int_vector) :: a, b, c

integer :: i, j
do i = 1, n
    a%elements(i) = i
    b%elements(i) = i
end do

c = int_add_vector(a, b)
print *, c%elements

instantiate matrix_t(integer, operator(+), n), &
    only: int_matrix => matrix, &
          int_add_matrix => add_matrix

type(int_matrix) :: am, bm, cm

do i = 1, n
    do j = 1, n
        am%elements(i,j) = i + j
        bm%elements(i,j) = i + j
    end do
end do

cm = int_add_matrix(am, bm)

print *, cm%elements

end