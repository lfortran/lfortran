module template_matrix_02_m

    implicit none
    private
    public :: matrix_t

    requirement elemental_op(t, op)
        type, deferred :: t
        pure elemental function op(l, r) result(rs)
            type(t), intent(in) :: l, r
            type(t) :: rs
        end function
    end requirement

    template matrix_t(t, plus, times, n)
        require :: elemental_op(t, plus), elemental_op(t, times)
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
            r%elements = plus(a%elements, b%elements)
        end function

        !pure function mul_matrix(a, b) result(r)
        !    type(matrix), intent(in) :: a, b
        !    type(matrix) :: r
!
        !    type(t) :: dot(n)
        !    type(t) :: temp
        !    integer :: i, j, k
!
        !    do i = 1, n
        !        do j = 1, n
        !            dot = times(a%elements(i,:), b%elements(:,j))
        !            temp = dot(1)
        !            do k = 2, n
        !                temp = plus(temp, dot(i))
        !            end do
        !            r%elements(i,j) = temp
        !        end do
        !    end do
        !end function
    end template

end module

program template_matrix_02
use template_matrix_02_m

integer, parameter :: n = 2
integer :: i, j

instantiate matrix_t(integer, operator(+), operator(*), n), &
    only: int_matrix => matrix, &
          int_add_matrix => add_matrix
          !int_mul_matrix => mul_matrix

type(int_matrix) :: am, bm, cm, dm

do i = 1, n
    do j = 1, n
        am%elements(i,j) = i
        bm%elements(i,j) = i
    end do
end do

cm = int_add_matrix(am, bm)
!dm = int_mul_matrix(am, bm)

print *, cm
!print *, cm%elements(1,1), cm%elements(1,2)
!print *, cm%elements(2,1), cm%elements(2,2)

end program