module derived_types_100_mod
  implicit none

  type :: person
    character(len=50) :: name
    integer, allocatable :: scores(:)
  end type person

contains

  function update_person(original) result(updated)
    type(person), pointer, intent(in) :: original
    type(person), pointer :: updated
    integer :: n

    allocate(updated)
    updated%name = "MODIFIED: " // trim(original%name)

    n = size(original%scores)
    allocate(updated%scores(n+1))
    updated%scores(1:n) = original%scores
    updated%scores(n+1) = 999
  end function update_person

end module derived_types_100_mod


program derived_types_100
  use derived_types_100_mod
  implicit none

  type(person), pointer :: x

  allocate(x)
  x%name = "John Doe"
  allocate(x%scores(3))
  x%scores = [85, 92, 78]

  x => update_person(x)

 print *, x%name 
 print *, x%scores

 if ( x%name /=  "MODIFIED: John Doe") error stop "Test failed: name mismatch"
 if ( size(x%scores) /= 4 ) error stop "Test failed: scores size mismatch"
 if ( any(x%scores /= [85, 92, 78, 999]) ) error stop "Test failed: last score mismatch"
    
end program derived_types_100
