module interface_33_bounds_mod
   integer :: lb = 1
end module interface_33_bounds_mod

module interface_33_iface_mod
   interface
      subroutine sub_with_bounds(arr)
         use interface_33_bounds_mod
         real :: arr(lb:)
      end subroutine sub_with_bounds
   end interface
end module interface_33_iface_mod
