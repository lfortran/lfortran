module m_version_defined_op_match_01
  implicit none
  private
  public :: version_t

  type :: version_t
     integer, allocatable :: num(:)
   contains
     generic :: operator(.match.) => match
     procedure, private :: match
  end type version_t

contains

  logical function match(this, other)
    class(version_t), intent(in) :: this, other

    if (.not. allocated(this%num) .or. .not. allocated(other%num)) then
       match = .false.
    elseif (size(this%num) /= size(other%num)) then
       match = .false.
    else
       match = all(this%num == other%num)
    end if
  end function match

end module m_version_defined_op_match_01


program defined_op_match_01
  use m_version_defined_op_match_01
  implicit none

  type(version_t) :: v1, v2
  if (v1 .match. v2) error stop

end program defined_op_match_01
