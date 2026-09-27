! External procedures with saved coarrays, compiled on their own; see
! coarrays_55.f90. The caller passes the other image's index in.
subroutine coarrays_55_never()
    implicit none
    integer, save :: cn[*] = 3
    cn = cn + 1
end subroutine coarrays_55_never

subroutine coarrays_55_a(other)
    implicit none
    integer, intent(in) :: other
    integer, save :: ca[*] = 10
    if (ca[other] /= 10) error stop 1
end subroutine coarrays_55_a

subroutine coarrays_55_b(other)
    implicit none
    integer, intent(in) :: other
    integer, save :: cb[*] = 20
    if (cb[other] /= 20) error stop 2
end subroutine coarrays_55_b
