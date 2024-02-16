module lfortran_intrinsic_kind
implicit none

contains

integer function selected_real_kind(P) result(res)
integer, intent(in) :: P
if (P < 7) then
    res = 4
else
    res = 8
end if
end function

integer function selected_char_kind(R) result(res)
character(len=*), intent(in) :: R
res = 1
end function

end module
