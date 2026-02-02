program implied_do_loops18
   implicit none

   type :: string_t
      character(len=:), allocatable :: s
   end type string_t

   type(string_t), allocatable :: file_names(:)
   logical, allocatable :: is_source(:)

   integer :: i

   allocate(file_names(3))

   file_names(1)%s = "main.f90"
   file_names(2)%s = ".gitignore"
   file_names(3)%s = "utils.f90"

   i = 100
   is_source = [(is_hidden_file(basename(file_names(i)%s)), i = 1, size(file_names))]

   do i = 1, size(is_source)
      print *, trim(file_names(i)%s), " -> ", is_source(i)
   end do

   if (any(is_source .neqv. [.false., .true., .false.])) error stop

contains
   function basename(path) result(tmp)
      character(len=*), intent(in) :: path
      character(len=:), allocatable :: tmp
      tmp = path
   end function basename

   logical function is_hidden_file(name)
      character(len=*), intent(in) :: name
      is_hidden_file = (len(name) > 0 .and. name(1:1) == ".")
   end function is_hidden_file

end program implied_do_loops18
