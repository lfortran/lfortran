module m_cli2

implicit none

contains

subroutine a2d(valu)
    doubleprecision, intent(out)  :: valu
    integer :: ivalu
    valu = real(ivalu, kind=kind(0.0d0))
end subroutine a2d

end module m_cli2
