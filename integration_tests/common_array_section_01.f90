! Array sections of COMMON arrays: the implied bounds of `a(:)` and the
! implied length of `s(2:)` must refer to the COMMON block storage.
subroutine update_date()
    implicit none
    integer :: idatef
    common /time/ idatef(2)
    idatef(:) = 1
end subroutine update_date

subroutine shift_date()
    implicit none
    integer :: idatef
    common /time/ idatef(2)
    idatef(2:) = idatef(:1) + 5
end subroutine shift_date

program common_array_section_01
    implicit none
    integer :: idatef(2)
    character(len=5) :: s
    common /time/ idatef
    common /text/ s
    idatef = 0
    call update_date()
    if (any(idatef /= 1)) error stop
    call shift_date()
    if (idatef(1) /= 1) error stop
    if (idatef(2) /= 6) error stop
    if (sum(idatef(:)) /= 7) error stop
    s = "hello"
    if (s(2:) /= "ello") error stop
    print *, idatef(:), s(2:)
end program common_array_section_01
