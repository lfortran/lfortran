program interface_33
   use interface_33_iface_mod, only: sub_with_bounds
   real :: a(4)
   a = [10.0, 20.0, 30.0, 40.0]
   call sub_with_bounds(a)
end program interface_33

subroutine sub_with_bounds(arr)
   use interface_33_bounds_mod
   real :: arr(lb:)
   if (arr(1) /= 10.0) error stop
   if (arr(2) /= 20.0) error stop
   if (arr(3) /= 30.0) error stop
   if (arr(4) /= 40.0) error stop
   print *, "PASSED"
end subroutine sub_with_bounds
