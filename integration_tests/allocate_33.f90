module allocate_33_mod
  implicit none
  type :: person
    character(len=50) :: name
    integer, allocatable :: scores(:)
  end type person
contains
  function update_person(original) result(updated)
    type(person), allocatable, intent(in) :: original
    type(person), allocatable :: updated
    integer :: num_scores
    allocate(updated)
    updated%name = "modified: " // trim(original%name)
    num_scores = size(original%scores)
    allocate(updated%scores(num_scores + 1))
    if (num_scores > 0) then
      updated%scores(1:num_scores) = original%scores + 10
    end if
    updated%scores(num_scores + 1) = 999
  end function update_person
end module allocate_33_mod

program allocate_33
  use allocate_33_mod   
  implicit none
  type(person), allocatable :: x
  
  allocate(x)
  x%name = "john doe"
  allocate(x%scores(3))
  x%scores = [85, 92, 78]
  
  x = update_person(x)
  
  if (x%scores(1) /= 95) error stop
  if (x%scores(2) /= 102) error stop
  if (x%scores(3) /= 88) error stop
  if (x%scores(4) /= 999) error stop

  print *, "All tests passed."
  
end program allocate_33