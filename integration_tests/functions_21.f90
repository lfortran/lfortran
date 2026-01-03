pure integer function my_merge(val1, val2, mask) result(merged_value)
  integer, intent(in) :: val1, val2
  logical, intent(in) :: mask

  if (mask) then
    merged_value = val1
  else
    merged_value = val2
  end if

end function my_merge


program functions_21
complex, dimension(3, 3) :: y
complex :: res(3)

res = mean_2_cdp_cdp(y, 1, mask=.true.)
print *, res
print *, abs(sum(res))
if (abs(abs(sum(res)) - 526.008179) > 1e-8) error stop 

contains
function mean_2_cdp_cdp(x, dim, mask) result(res)
  complex, intent(in) :: x(:,:)
  integer, intent(in) :: dim
  logical, intent(in), optional :: mask
  complex :: res(my_merge(size(x, 1), size(x, 2), mask=1<dim))

  interface
    pure integer function my_merge(val1, val2, mask)
      implicit none
      integer, intent(in) :: val1, val2
      logical, intent(in) :: mask
    end function my_merge
  end interface

  res = (123.41, -124.55)

end function mean_2_cdp_cdp
end program

