! global_init_19's module array pointers, initially associated with a whole
! array and with an array section, in one file with the program that uses
! the modules; global_init_26_n holds a section pointer alone. GFortran drops
! these initial targets, see global_init_19.f90, so this test is not labelled
! gfortran.
module global_init_26_m
    implicit none
    integer, target :: tgt(4) = [1, 2, 3, 4]
    integer, pointer :: pt(:) => tgt
    integer, pointer :: psec(:) => tgt(2:3)
end module global_init_26_m

module global_init_26_n
    implicit none
    integer, target :: tgt2(4) = [5, 6, 7, 8]
    integer, pointer :: psec2(:) => tgt2(2:3)
end module global_init_26_n

program global_init_26
    use global_init_26_m
    use global_init_26_n
    implicit none
    if (.not. associated(pt, tgt)) error stop 1
    if (size(pt) /= 4 .or. sum(pt) /= 10) error stop 2
    ! associated(psec, tgt(2:3)) is avoided; see global_init_19_s.f90.
    if (.not. associated(psec)) error stop 3
    if (size(psec) /= 2 .or. lbound(psec, 1) /= 1) error stop 4
    if (sum(psec) /= 5) error stop 5
    psec(1) = 20
    if (tgt(2) /= 20) error stop 6
    if (.not. associated(psec2)) error stop 7
    if (size(psec2) /= 2 .or. lbound(psec2, 1) /= 1) error stop 8
    if (sum(psec2) /= 13) error stop 9
    psec2(2) = 70
    if (tgt2(3) /= 70) error stop 10
    print *, "ok"
end program global_init_26
