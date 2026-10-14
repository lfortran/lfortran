! Blank COMMON may have a different size in each scoping unit, including a
! module. Objects are associated by storage position, also when a unit
! gives an object the name of an object at another position elsewhere.
module common_56_mod
    implicit none
    integer :: head
    common head
end module common_56_mod

subroutine common_56_set()
    implicit none
    integer :: a
    common a
    a = 5
end subroutine common_56_set

subroutine common_56_grow()
    implicit none
    integer :: x, a
    common x
    common a
    x = 1
    a = 2
    call common_56_set()
    if (x /= 5) error stop
    if (a /= 2) error stop
end subroutine common_56_grow

! An array that starts inside the layout of the other units and extends
! past its end.
subroutine common_56_overlay()
    implicit none
    integer :: w(3)
    common w
    if (w(1) /= 5) error stop
    if (w(2) /= 2) error stop
    w(3) = 3
end subroutine common_56_overlay

! EQUIVALENCE extends the unit's layout of the block past its last member.
subroutine common_56_equivalence()
    implicit none
    integer :: h, ext(3)
    common h
    equivalence (h, ext(1))
    if (any(ext /= [5, 2, 3])) error stop
    ext(3) = 4
end subroutine common_56_equivalence

! A named block may have the name of one of its objects.
subroutine common_56_same_name()
    implicit none
    integer :: common_56_tag
    common /common_56_tag/ common_56_tag
    common_56_tag = 11
end subroutine common_56_same_name

program common_56
    use common_56_mod, only: head
    implicit none
    integer :: y(3), tag
    common y
    common /common_56_tag/ tag
    call common_56_grow()
    call common_56_overlay()
    call common_56_equivalence()
    call common_56_same_name()
    print *, head, y, tag
    if (head /= 5) error stop
    if (any(y /= [5, 2, 4])) error stop
    if (tag /= 11) error stop
end program common_56
