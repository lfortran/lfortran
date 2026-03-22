#define WRAP(exp, desc) result = (exp)

program cpp_pre_10
  implicit none
  logical :: result
  result = .true.
  ! ) inside a string must not terminate macro argument collection early
  WRAP(.true., "string with ) paren inside")
  ! , inside a string must not split macro arguments incorrectly
  WRAP(.true., "string with , comma inside")
  ! single-quoted strings with ) or , must also work
  WRAP(.true., 'single-quoted ) paren')
  if (.not. result) error stop
end program
