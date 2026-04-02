module select_type_class_array_01_mod
  implicit none
  type :: my_type
    integer :: val = 0
  end type
contains
  subroutine process(data)
    class(*), dimension(..), intent(inout) :: data

    select rank(data)
    rank(2)
      select type(data)
      type is(my_type)
        data(1,1)%val = 42
        data(2,1)%val = 10
      end select
    end select
  end subroutine
end module

program select_type_class_array_01
  use select_type_class_array_01_mod
  implicit none
  type(my_type) :: arr(2,2)

  call process(arr)

  if (arr(1,1)%val /= 42) error stop
  if (arr(2,1)%val /= 10) error stop

  print *, "PASS"
end program
