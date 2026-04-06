module gpu_metal_134_m
implicit none
interface
  elemental module function relu(x) result(y)
    real, intent(in) :: x
    real :: y
  end function
end interface
end module
