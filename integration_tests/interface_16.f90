module interface_16_module

interface all_close

procedure :: all_close_1_csp
procedure :: all_close_2_csp
end interface all_close
contains

logical pure function all_close_1_csp(a, b) result(close)

    complex, intent(in) :: a(:), b(:)
end function all_close_1_csp
logical pure function all_close_2_csp(a, b) result(close)

    complex, intent(in) :: a(:,:), b(:,:)
    close = .false.
end function all_close_2_csp

end module

program interface_16
use interface_16_module, only: all_close

complex :: z(4, 4)

print *, all_close(z + cmplx(1.0e-11, 1.0e-11), z)
if (all_close(z + cmplx(1.0e-11, 1.0e-11), z)) error stop

end program interface_16
  
