module template_02_m
  implicit none

  requirement eq(t, ne)
    type, deferred :: t
    function ne(lhs, rhs)
      type(t), intent(in) :: lhs, rhs
      logical :: ne
    end function
  end requirement

  template change_positions_tmpl(t, ne)
    requires eq(t, ne)
    private
    public :: change_positions_t
  contains
    function change_positions_t(vec) result(mask)
      type(t), intent(in) :: vec(:)
      integer, allocatable :: pos(:), mask(:)
      integer :: i, j, n
      n = size(vec)
      allocate (pos(n))
      if (n < 1) return
      pos = 0
      pos(1) = 1
      j = 1
      do i = 2,n
        if (ne(vec(i), vec(i-1))) then
          j = j+1
          pos(j) = i
        end if
      end do
      allocate(mask(j))
      do i = 1,j
        mask(i) = pos(i)
      end do
    end function
  end template

end module

program template_02
  use template_02_m
  implicit none
  instantiate change_positions_tmpl(integer, operator(/=)), &
    only: change_positions_int => change_positions_t
  instantiate change_positions_tmpl(character, operator(/=)), &
    only: change_positions_chr => change_positions_t
  print *, change_positions_int([3, 3, 6, 2, 2, 2, 1])
  print *, change_positions_chr(["a", "a", "b", "p", "p", "p", "o"])
end program