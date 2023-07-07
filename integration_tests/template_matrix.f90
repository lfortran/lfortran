module template_matrix_m
  implicit none
  private
  public :: matrix_t

  requirement elemental_oper(t, op)

    type, deferred :: t
    elemental function op(x, y) result(z)
      type(t), intent(in) :: x, y
      type(t) :: z
    end function

  end requirement

  template matrix_t(t, plus, times, n)

    requires elemental_oper(t, plus)
    requires elemental_oper(t, times)
    integer :: n
    private

    public :: matrix

    type :: matrix
      type(t) :: elements(n,n)
    end type

  contains

    elemental subroutine plus_matrix(m_x, m_y, m_z)
      type(matrix), intent(in) :: m_x, m_y
      type(matrix), intent(inout) :: m_z   
      m_z%elements = plus(m_x%elements, m_y%elements)
end subroutine


    !elemental function matmul_matrix(m_x, m_y) result(m_z)
    !  type(matrix), intent(in) :: m_x, m_y
    !  type(matrix) :: m_z
!
    !  type(t) :: tmp
    !  integer :: i, j, k
!
    !  do i= 1,n
    !    do j= 1,n
    !      tmp = m_x%elements(i,1)*m_y%elements(1,j)
    !      do k = 2, n
    !          tmp = plus(tmp, times(m_x%elements(i,k),m_y%elements(k,j)))
    !      end do
    !      m_z%elements(i,j) = tmp
    !    end do
    !  end do
    !end function

  end template

contains

  elemental function plus_integer(x, y) result(z)
    integer, intent(in) :: x, y
    integer :: z
    z = x + y
  end function

  elemental function mult_integer(x, y) result(z)
    integer, intent(in) :: x, y
    integer :: z
    z = x * y
  end function

  elemental function plus_complex(x, y) result(z)
    complex, intent(in) :: x, y
    complex :: z
    z = x + y
  end function

  elemental function mult_complex(x, y) result(z)
    complex, intent(in) :: x, y
    complex :: z
    z = x * y
  end function

  subroutine test_template()
    integer, parameter :: n = 2
    instantiate matrix_t(integer, plus_integer, mult_integer, n), &
      only: integer_matrix => matrix, &
            integer_plus_matrix => plus_matrix
    type(integer_matrix) :: m1, m2, m3
    m1%elements(1,1) = 1
    m1%elements(1,2) = 0
    m1%elements(2,1) = 0
    m1%elements(2,2) = 1

    m2%elements(1,1) = 1
    m2%elements(1,2) = 2
    m2%elements(2,1) = 2
    m2%elements(2,2) = 1

    call integer_plus_matrix(m1, m2, m3)

    print *, m1%elements
    print *, m2%elements
    print *, m3%elements
  end subroutine

end module

program template_matrix
use template_matrix_m

implicit none

call test_template()

end program template_matrix