! Array results of every intrinsic element type cross a runtime slot: integer,
! complex, logical and real of non-default kinds, constant-length character,
! and ranks one to three, with an additional argument.
module traits_runtime_array_result_02_m
    implicit none
    abstract interface :: IShapes
        function grid(self, m) result(x)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: m
            integer(8), allocatable :: x(:,:)
        end function
        function waves(self) result(z)
            type(itself), pass, intent(in) :: self
            complex(4), allocatable :: z(:)
        end function
        function flags(self) result(b)
            type(itself), pass, intent(in) :: self
            logical, allocatable :: b(:)
        end function
        function names(self) result(c)
            type(itself), pass, intent(in) :: self
            character(len=3), allocatable :: c(:)
        end function
        function cube(self) result(r)
            type(itself), pass, intent(in) :: self
            real(4), allocatable :: r(:,:,:)
        end function
        function bytes(self) result(b)
            type(itself), pass, intent(in) :: self
            integer(1), allocatable :: b(:)
        end function
        function pairs(self) result(z)
            type(itself), pass, intent(in) :: self
            complex(8), allocatable :: z(:,:)
        end function
        function bits(self) result(l)
            type(itself), pass, intent(in) :: self
            logical(1), allocatable :: l(:)
        end function
    end interface
    type :: Box
        integer :: n = 2
    end type
    implements IShapes :: Box
        procedure, pass :: grid => box_grid
        procedure, pass :: waves => box_waves
        procedure, pass :: flags => box_flags
        procedure, pass :: names => box_names
        procedure, pass :: cube => box_cube
        procedure, pass :: bytes => box_bytes
        procedure, pass :: pairs => box_pairs
        procedure, pass :: bits => box_bits
    end implements
contains
    function box_grid(self, m) result(x)
        class(Box), intent(in) :: self
        integer, intent(in) :: m
        integer(8), allocatable :: x(:,:)
        integer :: i, j
        allocate(x(self%n, m))
        do j = 1, m
            do i = 1, self%n
                x(i, j) = 10 * i + j
            end do
        end do
    end function
    function box_waves(self) result(z)
        class(Box), intent(in) :: self
        complex(4), allocatable :: z(:)
        allocate(z(self%n))
        z = (1.0, -1.0) * self%n
    end function
    function box_flags(self) result(b)
        class(Box), intent(in) :: self
        logical, allocatable :: b(:)
        allocate(b(self%n + 1))
        b = .true.
        b(1) = .false.
    end function
    function box_names(self) result(c)
        class(Box), intent(in) :: self
        character(len=3), allocatable :: c(:)
        allocate(c(self%n))
        c = 'abc'
        c(1) = 'xyz'
    end function
    function box_cube(self) result(r)
        class(Box), intent(in) :: self
        real(4), allocatable :: r(:,:,:)
        allocate(r(self%n, 2, 3))
        r = 1.5
    end function
    function box_bytes(self) result(b)
        class(Box), intent(in) :: self
        integer(1), allocatable :: b(:)
        allocate(b(self%n))
        b = -7_1
    end function
    function box_pairs(self) result(z)
        class(Box), intent(in) :: self
        complex(8), allocatable :: z(:,:)
        allocate(z(self%n, self%n))
        z = (0.5d0, 2d0)
    end function
    function box_bits(self) result(l)
        class(Box), intent(in) :: self
        logical(1), allocatable :: l(:)
        allocate(l(4))
        l = .false.
        l(self%n) = .true.
    end function
    subroutine check(v)
        class(IShapes), intent(in) :: v
        integer(8), allocatable :: g(:,:)
        complex(4), allocatable :: z(:)
        logical, allocatable :: b(:)
        character(len=3), allocatable :: c(:)
        character(len=5), allocatable :: c5(:)
        real(4), allocatable :: r(:,:,:)
        integer(1), allocatable :: i1(:)
        complex(8), allocatable :: z8(:,:)
        logical(1), allocatable :: l1(:)
        g = v%grid(3)
        if (any(shape(g) /= [2, 3])) error stop 1
        if (g(2, 3) /= 23_8 .or. sum(v%grid(4)) /= 2*(1+2+3+4) + 4*(10+20)) error stop 2
        if (size(v%grid(5), 2) /= 5) error stop 3
        z = v%waves()
        if (size(z) /= 2 .or. any(z /= (2.0, -2.0))) error stop 4
        b = v%flags()
        if (size(b) /= 3 .or. b(1) .or. .not. b(3)) error stop 5
        if (count(v%flags()) /= 2) error stop 6
        c = v%names()
        if (size(c) /= 2 .or. c(1) /= 'xyz' .or. c(2) /= 'abc') error stop 7
        ! A target of another length keeps its length.
        allocate(c5(2))
        c5 = v%names()
        if (len(c5) /= 5 .or. c5(1) /= 'xyz  ' .or. c5(2) /= 'abc  ') error stop 13
        r = v%cube()
        if (any(shape(r) /= [2, 2, 3]) .or. sum(r) /= 18.0) error stop 8
        if (sum(v%cube() * 2) /= 36.0) error stop 9
        i1 = v%bytes()
        if (size(i1) /= 2 .or. any(i1 /= -7_1) .or. kind(i1) /= 1) error stop 10
        z8 = v%pairs()
        if (any(shape(z8) /= [2, 2]) .or. sum(v%pairs()) /= (2d0, 8d0)) error stop 11
        l1 = v%bits()
        if (size(l1) /= 4 .or. count(l1) /= 1 .or. .not. l1(2)) error stop 12
        print *, v%names()
    end subroutine
end module traits_runtime_array_result_02_m

program traits_runtime_array_result_02
    use traits_runtime_array_result_02_m
    implicit none
    class(IShapes), allocatable :: o
    call check(Box())
    o = Box()
    call check(o)
    print '(a)', 'traits_runtime_array_result_02 ok'
end program traits_runtime_array_result_02
