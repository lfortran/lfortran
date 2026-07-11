! Multi-rank and parent-type cases: non-elemental type-bound assignment(=)
! with scalar dummies does not match array LHS (intrinsic assignment).

module derived_types_152_mod
   implicit none

   type :: t2
      integer, pointer :: leaf => null()
      logical :: is_temporary = .false.
   contains
      procedure :: assign_t2
      generic :: assignment(=) => assign_t2
   end type t2

   type :: parent_t
      integer, pointer :: leaf => null()
      logical :: is_temporary = .false.
   contains
      procedure :: assign_parent
      generic :: assignment(=) => assign_parent
   end type parent_t

   type, extends(parent_t) :: child_t
      integer :: tag = 0
   end type child_t

contains

   subroutine assign_t2(lhs, rhs)
      class(t2), intent(inout) :: lhs
      class(t2), intent(in)    :: rhs
      lhs%leaf => rhs%leaf
      lhs%is_temporary = .true.
   end subroutine assign_t2

   subroutine assign_parent(lhs, rhs)
      class(parent_t), intent(inout) :: lhs
      class(parent_t), intent(in)    :: rhs
      lhs%leaf => rhs%leaf
      lhs%is_temporary = .true.
   end subroutine assign_parent

end module derived_types_152_mod

program derived_types_152
   use derived_types_152_mod
   implicit none

   call test_rank2()
   call test_parent_assign()

contains

   subroutine test_rank2()
      type(t2), dimension(2, 3) :: src, dst
      type(t2) :: scalar
      integer :: i, j

      do j = 1, 3
         do i = 1, 2
            allocate(src(i, j)%leaf)
            src(i, j)%leaf = 10 * i + j
            src(i, j)%is_temporary = .false.
            dst(i, j)%is_temporary = .true.
         end do
      end do

      dst = src
      do j = 1, 3
         do i = 1, 2
            if (.not. associated(dst(i, j)%leaf, src(i, j)%leaf)) error stop 101
            if (dst(i, j)%leaf /= 10 * i + j) error stop 102
            if (dst(i, j)%is_temporary) error stop 103
         end do
      end do

      do j = 1, 3
         do i = 1, 2
            dst(i, j)%leaf => null()
            dst(i, j)%is_temporary = .true.
         end do
      end do
      dst(1:2, 1:3) = src(1:2, 1:3)
      do j = 1, 3
         do i = 1, 2
            if (.not. associated(dst(i, j)%leaf, src(i, j)%leaf)) error stop 104
            if (dst(i, j)%is_temporary) error stop 105
         end do
      end do

      allocate(scalar%leaf)
      scalar%leaf = 99
      scalar%is_temporary = .false.
      dst = scalar
      do j = 1, 3
         do i = 1, 2
            if (.not. associated(dst(i, j)%leaf, scalar%leaf)) error stop 106
            if (dst(i, j)%leaf /= 99) error stop 107
            if (dst(i, j)%is_temporary) error stop 108
         end do
      end do

      do j = 1, 3
         do i = 1, 2
            deallocate(src(i, j)%leaf)
         end do
      end do
      deallocate(scalar%leaf)
   end subroutine test_rank2

   subroutine test_parent_assign()
      type(child_t), dimension(3) :: a, b
      integer :: i

      do i = 1, 3
         allocate(a(i)%leaf)
         a(i)%leaf = i * 7
         a(i)%is_temporary = .false.
         a(i)%tag = i
         b(i)%is_temporary = .true.
         b(i)%tag = -1
      end do

      ! Array assignment of extended type: parent component uses defined
      ! assignment (is_temporary set); extension components still copy.
      b = a
      do i = 1, 3
         if (.not. associated(b(i)%leaf, a(i)%leaf)) error stop 201
         if (b(i)%leaf /= i * 7) error stop 202
         if (.not. b(i)%is_temporary) error stop 203
      end do

      do i = 1, 3
         b(i)%leaf => null()
         b(i)%is_temporary = .false.
      end do
      b(1:3) = a(1:3)
      do i = 1, 3
         if (.not. associated(b(i)%leaf, a(i)%leaf)) error stop 204
         if (b(i)%leaf /= i * 7) error stop 205
         if (.not. b(i)%is_temporary) error stop 206
      end do

      ! Scalar child assignment uses parent defined assignment
      b(1)%is_temporary = .false.
      b(1) = a(2)
      if (.not. associated(b(1)%leaf, a(2)%leaf)) error stop 207
      if (.not. b(1)%is_temporary) error stop 208

      do i = 1, 3
         deallocate(a(i)%leaf)
      end do
   end subroutine test_parent_assign

end program derived_types_152
