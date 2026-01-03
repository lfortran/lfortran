module utils
implicit none

contains

subroutine stop_error(msg)

character(len=*) :: msg ! Message to print on stdout
print *, msg
end subroutine

end module

module error_util
use utils, only: stop_error
implicit none

contains

integer function error(err, eps) result(r)
real(8), intent(in) :: err, eps
print "('Test failed: error = ', es10.2, '   > ', es10.2, ' specified.')", &
        err, eps
call stop_error("Aborting...")
r = 0
end function

end module

program functions_38
use error_util, only: error
implicit none

call driver()

contains

subroutine driver()
    integer :: tmp
    tmp = error(1.0_8, 0.0_8)
end subroutine

end program
