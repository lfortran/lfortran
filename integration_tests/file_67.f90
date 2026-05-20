program file_67
  ! Test sequential unformatted I/O with array reads (float, double, logical)
  ! Verifies that array read functions properly maintain record state
  implicit none
  integer :: a
  logical :: l1
  logical :: larr(2), larr_read(2)
  real :: rarr(2), rarr_read(2)
  double precision :: darr(3), darr_read(3)

  l1 = .true.
  a = 42
  rarr = (/1.5, 2.5/)
  larr = (/.true., .false./)
  darr = (/3.14d0, 2.71d0, 1.41d0/)

  ! Write multiple records with mixed scalar + array data
  open(8, file="file_67.dat", form="unformatted", access="sequential", status="replace")
  write(8) a, l1
  write(8) a, rarr
  write(8) a, larr
  write(8) darr
  close(8)

  ! Read them back
  open(8, file="file_67.dat", form="unformatted", access="sequential", status="old")
  read(8) a, l1
  if (a /= 42) error stop
  if (.not. l1) error stop

  read(8) a, rarr_read
  if (a /= 42) error stop
  if (abs(rarr_read(1) - 1.5) > 1.0e-6) error stop
  if (abs(rarr_read(2) - 2.5) > 1.0e-6) error stop

  read(8) a, larr_read
  if (a /= 42) error stop
  if (.not. larr_read(1)) error stop
  if (larr_read(2)) error stop

  read(8) darr_read
  if (abs(darr_read(1) - 3.14d0) > 1.0d-10) error stop
  if (abs(darr_read(2) - 2.71d0) > 1.0d-10) error stop
  if (abs(darr_read(3) - 1.41d0) > 1.0d-10) error stop

  close(8, status="delete")
  print *, "PASS"
end program
