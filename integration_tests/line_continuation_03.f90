program line_continuation_03
! The same as line_continuation_02, but we do use & to split a token
implicit none

! Here are the rules for & based on the § 6.3.2.4 ("Free form statement
! continuation") in Fortran 2018 standard.

! The & character in a comment has no effect

! The & character can be used to continue a line like this:

integer &
    :: i

! If it is used between tokens, then one can, but does not have to put another &
! on the next line:

integer &
    &:: k


i = 5

print * &
    , i

! One can continue strings also by treating them as tokens:

print *, "some string &
    &is continued"

! Unless the & is at the end of the line, it can be used as a regular
! character in a string:

print *, "some string can contain & as a regular character &
    &is continued"

print *, "Even here: &&
    && <- there will be two &"

! One can put arbitrary comments and empty lines after & that will be skipped:

print *, &

    i

print *, & ! comment 1
! comment 2

! comment 3

    i

print *, & ! comment 1
! comment 2

! comment 3

    &i

! In strings the comment cannot be after the first &, but it can be on
! subsequent lines:

print *, "some string &
! comment 2

! comment 3

    &is continued"

print *, '*t',&
&''
!!     call set_args ( \'  &
!! & \n

end program
