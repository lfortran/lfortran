program read_88
   implicit none

   type :: spec_t
      integer :: num
   end type spec_t
   type :: basis_t
      type(spec_t), allocatable :: spec(:)
      integer :: nspec = 3
   end type basis_t

   type(basis_t) :: basis
   character(40) :: line = "garbage 11 22 33 trailing"
   integer :: j, pos_i, pos_f, unit_no

   allocate(basis%spec(3))

   pos_i = 9
   pos_f = 16
   read(line(pos_i:pos_f),*) (basis%spec(j)%num, j=1,basis%nspec)
   if (basis%spec(1)%num /= 11) error stop 1
   if (basis%spec(2)%num /= 22) error stop 2
   if (basis%spec(3)%num /= 33) error stop 3

   basis%spec(:)%num = 0
   open(newunit=unit_no, status='scratch', form='formatted')
   write(unit_no, '(a)') "44 55 66"
   rewind(unit_no)
   read(unit_no, *) (basis%spec(j)%num, j=1,basis%nspec)
   close(unit_no)
   if (basis%spec(1)%num /= 44) error stop 4
   if (basis%spec(2)%num /= 55) error stop 5
   if (basis%spec(3)%num /= 66) error stop 6
end program read_88
