module component_section_mod_03
  implicit none

  type :: point
    real(8) :: x(3)
    integer :: province
  end type

  interface ordered
    module procedure ordered_real
  end interface

contains

  logical function ordered_real(values, n)
    integer, intent(in) :: n
    real(8), intent(in) :: values(n)

    ordered_real = all(values(2:n) >= values(1:n-1))
  end function

  subroutine bump(values)
    real(8), intent(inout) :: values(:)

    values = values + 10.0d0
  end subroutine

  subroutine read_provinces(record, points, row)
    character(len=*), intent(in) :: record
    type(point), allocatable, intent(inout) :: points(:,:)
    integer, intent(in) :: row

    read(record, *) points(:, row)%province
  end subroutine
end module

program derived_component_section_03
  use component_section_mod_03
  implicit none

  type(point), allocatable :: points(:)
  integer :: location(1)

  allocate(points(3))
  points(1)%x = [1.0d0, 4.0d0, 7.0d0]
  points(2)%x = [2.0d0, 5.0d0, 8.0d0]
  points(3)%x = [3.0d0, 6.0d0, 9.0d0]

  if (.not. ordered(points(:)%x(1), size(points))) error stop

  location = maxloc(points(:)%x(2), points(:)%x(2) < 6.0d0)
  if (location(1) /= 2) error stop

  if (any(isnan(points(:)%x(3)))) error stop
  call bump(points(:)%x(1))
  if (any(points(:)%x(1) /= [11.0d0, 12.0d0, 13.0d0])) error stop


  print *, "pass"
end program
