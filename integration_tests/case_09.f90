! Test for https://github.com/lfortran/lfortran/issues/8407
! Select case with multiple range conditions in a single case
! Exact MRE from issue body
program casetest
  implicit none
  integer :: i
  do i = 1,5
     select case (i)
     case (2:3,5)
        print "(A,I0)",'case (2:3,5) i=',i
     case default
        print "(A,I0)",'case default i=',i
     end select
  end do
  if (.not. check()) error stop
contains
  logical function check()
    integer :: j
    check = .true.
    do j = 1,5
       select case (j)
       case (2:3,5)
          if (j /= 2 .and. j /= 3 .and. j /= 5) check = .false.
       case default
          if (j /= 1 .and. j /= 4) check = .false.
       end select
    end do
  end function check
end program casetest
