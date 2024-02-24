module lfortran_intrinsic_kind
implicit none

contains

integer function selected_char_kind(R) result(res)
character(len=*), intent(in) :: R
res = 1
end function

end module
