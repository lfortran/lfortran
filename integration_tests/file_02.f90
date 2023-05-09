program RewindExample
  implicit none
  integer :: i, j, stat
  character(len=20) :: filename = "a.txt"
  integer, parameter :: n = 3
  integer :: data(n)
  integer :: sum = 0.0

  ! Open the file for reading
  open(unit=10, file=filename, status="old", action="read", iostat=stat)
!   print *, stat
  if (stat /= 0) then
    write(*,*) "Error opening file: ", filename
    stop
  end if

  ! Read the data and compute the sum
  do i = 1, n
    j = 0
    read(10, *, iostat=stat) j
    ! print *, stat
    ! if (stat > 0) then
    !   write(*,*) "Error reading file: ", filename
    !   stop
    ! end if
    print *, j
    sum = sum + j
  end do

  ! Print the sum
  write(*,*) "Sum = ", sum

  ! Rewind the file
  rewind(10)

  ! Read the data and compute the sum again
  sum = 0.0
  do i = 1, n
    j = 0
    read(10, *, iostat=stat) j
    ! if (stat /= 0) then
    !   write(*,*) "Error reading file: ", filename
    !   stop
    ! end if
    sum = sum + j
  end do

  ! Print the sum again
  write(*,*) "Sum (after rewind) = ", sum

  ! Close the file
  close(10)

end program RewindExample
