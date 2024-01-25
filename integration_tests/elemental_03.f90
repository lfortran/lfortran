program elemental_03

real :: x(2) = [1, 2], y(2) = [1.0, 2.1]
logical :: close(2)
close = is_close(x, y, 1e-9)
print *, close
if (.not. close(1)) error stop
if (close(2)) error stop
close = is_close(x, y)
print *, close
if (.not. close(1)) error stop
if (close(2)) error stop


contains
elemental logical function is_close(a, b, rel_tol) result(close)
real, intent(in) :: a, b
real, intent(in), optional :: rel_tol
if (present(rel_tol)) then
  close = abs(a - b) <= rel_tol
else
    close = abs(a - b) <= 1e-9
end if

end function is_close

end program elemental_03
