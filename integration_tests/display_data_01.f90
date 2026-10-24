program display_data_01
! Outside a notebook, display_data and clear_output link and do nothing.
use lfortran_display, only: display_data, clear_output
implicit none
integer :: i
i = 0
call display_data("text/html", "<b>hello</b>")
call clear_output()
i = i + 1
if (i /= 1) error stop
print *, "done"
end program
