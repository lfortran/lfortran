! Standard-Fortran oracle for traits_generic_type_01.f90. Every generic
! instance is concretized by hand with the instance's spelling in its name
! (PairwiseSum{SimpleSum} -> PairwiseSum_SimpleSum), each generic method is
! a generic binding over its integer and real(real64) members, and every
! initializer is a generic interface named like its type. Sealed TYPE
! receivers become CLASS receivers because standard Fortran requires a
! polymorphic passed object for an extensible type; no extension exists, so
! dispatch is unchanged. Same values and the same check count (168).
module traits_generic_type_01_oracle_leaves
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: SimpleSum, ScaledSum

    type :: SimpleSum
    contains
        procedure, nopass :: sum_i => simple_sum_i
        procedure, nopass :: sum_r => simple_sum_r
        generic :: sum => sum_i, sum_r
    end type SimpleSum

    type :: ScaledSum
        integer :: factor = 1
    contains
        procedure, pass :: sum_i => scaled_sum_i
        procedure, pass :: sum_r => scaled_sum_r
        generic :: sum => sum_i, sum_r
    end type ScaledSum

contains

    function simple_sum_i(x) result(s)
        integer, intent(in) :: x(:)
        integer :: s, i
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum_i

    function simple_sum_r(x) result(s)
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: i
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum_r

    function scaled_sum_i(self, x) result(s)
        class(ScaledSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: s, i
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
        s = self%factor * s
    end function scaled_sum_i

    function scaled_sum_r(self, x) result(s)
        class(ScaledSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: i
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
        s = real(self%factor, real64) * s
    end function scaled_sum_r
end module traits_generic_type_01_oracle_leaves

module traits_generic_type_01_oracle_averager
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: IAverager

    type, abstract :: IAverager
    contains
        procedure(average_i_iface), deferred, pass :: average_i
        procedure(average_r_iface), deferred, pass :: average_r
        generic :: average => average_i, average_r
    end type IAverager

    abstract interface
        function average_i_iface(self, x) result(a)
            import :: IAverager
            class(IAverager), intent(in) :: self
            integer, intent(in) :: x(:)
            integer :: a
        end function average_i_iface
        function average_r_iface(self, x) result(a)
            import :: IAverager, real64
            class(IAverager), intent(in) :: self
            real(real64), intent(in) :: x(:)
            real(real64) :: a
        end function average_r_iface
    end interface
end module traits_generic_type_01_oracle_averager

! The instances the generic program creates in its main program.
module traits_generic_type_01_oracle_instances
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_type_01_oracle_leaves, only: SimpleSum, ScaledSum
    use traits_generic_type_01_oracle_averager, only: IAverager
    implicit none
    private
    public :: PairwiseSum_SimpleSum, PairwiseSum_PairwiseSum_SimpleSum
    public :: PairwiseSum_ScaledSum, Box_SimpleSum, Box_ScaledSum
    public :: PairwiseSum_Box_ScaledSum, Box_PairwiseSum_ScaledSum
    public :: Averager_PairwiseSum_SimpleSum, Averager_Box_ScaledSum
    public :: Averager_Box_SimpleSum, Averager_PairwiseSum_PairwiseSum_SimpleSum

    ! PairwiseSum{SimpleSum}
    type :: PairwiseSum_SimpleSum
        private
        type(SimpleSum), allocatable :: other
    contains
        procedure, pass :: sum_i => ps_s_sum_i
        procedure, pass :: sum_r => ps_s_sum_r
        generic :: sum => sum_i, sum_r
    end type PairwiseSum_SimpleSum
    interface PairwiseSum_SimpleSum
        module procedure ps_s_init
    end interface PairwiseSum_SimpleSum

    ! PairwiseSum{PairwiseSum{SimpleSum}}
    type :: PairwiseSum_PairwiseSum_SimpleSum
        private
        type(PairwiseSum_SimpleSum), allocatable :: other
    contains
        procedure, pass :: sum_i => ps_ps_sum_i
        procedure, pass :: sum_r => ps_ps_sum_r
        generic :: sum => sum_i, sum_r
    end type PairwiseSum_PairwiseSum_SimpleSum
    interface PairwiseSum_PairwiseSum_SimpleSum
        module procedure ps_ps_init
    end interface PairwiseSum_PairwiseSum_SimpleSum

    ! PairwiseSum{ScaledSum}
    type :: PairwiseSum_ScaledSum
        private
        type(ScaledSum), allocatable :: other
    contains
        procedure, pass :: sum_i => ps_c_sum_i
        procedure, pass :: sum_r => ps_c_sum_r
        generic :: sum => sum_i, sum_r
    end type PairwiseSum_ScaledSum
    interface PairwiseSum_ScaledSum
        module procedure ps_c_init
    end interface PairwiseSum_ScaledSum

    ! Box{SimpleSum}
    type :: Box_SimpleSum
        type(SimpleSum) :: item
        integer :: copies = 1
    contains
        procedure, pass :: sum_i => b_s_sum_i
        procedure, pass :: sum_r => b_s_sum_r
        generic :: sum => sum_i, sum_r
    end type Box_SimpleSum
    interface Box_SimpleSum
        module procedure b_s_make
    end interface Box_SimpleSum

    ! Box{ScaledSum}
    type :: Box_ScaledSum
        type(ScaledSum) :: item
        integer :: copies = 1
    contains
        procedure, pass :: sum_i => b_c_sum_i
        procedure, pass :: sum_r => b_c_sum_r
        generic :: sum => sum_i, sum_r
    end type Box_ScaledSum
    interface Box_ScaledSum
        module procedure b_c_make
    end interface Box_ScaledSum

    ! PairwiseSum{Box{ScaledSum}}
    type :: PairwiseSum_Box_ScaledSum
        private
        type(Box_ScaledSum), allocatable :: other
    contains
        procedure, pass :: sum_i => ps_bc_sum_i
        procedure, pass :: sum_r => ps_bc_sum_r
        generic :: sum => sum_i, sum_r
    end type PairwiseSum_Box_ScaledSum
    interface PairwiseSum_Box_ScaledSum
        module procedure ps_bc_init
    end interface PairwiseSum_Box_ScaledSum

    ! Box{PairwiseSum{ScaledSum}}
    type :: Box_PairwiseSum_ScaledSum
        type(PairwiseSum_ScaledSum) :: item
        integer :: copies = 1
    contains
        procedure, pass :: sum_i => b_pc_sum_i
        procedure, pass :: sum_r => b_pc_sum_r
        generic :: sum => sum_i, sum_r
    end type Box_PairwiseSum_ScaledSum
    interface Box_PairwiseSum_ScaledSum
        module procedure b_pc_make
    end interface Box_PairwiseSum_ScaledSum

    ! Averager{PairwiseSum{SimpleSum}}
    type, extends(IAverager) :: Averager_PairwiseSum_SimpleSum
        private
        type(PairwiseSum_SimpleSum), allocatable :: drv
    contains
        procedure, pass :: average_i => a_ps_average_i
        procedure, pass :: average_r => a_ps_average_r
    end type Averager_PairwiseSum_SimpleSum
    interface Averager_PairwiseSum_SimpleSum
        module procedure a_ps_init
    end interface Averager_PairwiseSum_SimpleSum

    ! Averager{Box{ScaledSum}}
    type, extends(IAverager) :: Averager_Box_ScaledSum
        private
        type(Box_ScaledSum), allocatable :: drv
    contains
        procedure, pass :: average_i => a_bc_average_i
        procedure, pass :: average_r => a_bc_average_r
    end type Averager_Box_ScaledSum
    interface Averager_Box_ScaledSum
        module procedure a_bc_init
    end interface Averager_Box_ScaledSum

    ! Averager{Box{SimpleSum}}
    type, extends(IAverager) :: Averager_Box_SimpleSum
        private
        type(Box_SimpleSum), allocatable :: drv
    contains
        procedure, pass :: average_i => a_bs_average_i
        procedure, pass :: average_r => a_bs_average_r
    end type Averager_Box_SimpleSum
    interface Averager_Box_SimpleSum
        module procedure a_bs_init
    end interface Averager_Box_SimpleSum

    ! Averager{PairwiseSum{PairwiseSum{SimpleSum}}}
    type, extends(IAverager) :: Averager_PairwiseSum_PairwiseSum_SimpleSum
        private
        type(PairwiseSum_PairwiseSum_SimpleSum), allocatable :: drv
    contains
        procedure, pass :: average_i => a_pps_average_i
        procedure, pass :: average_r => a_pps_average_r
    end type Averager_PairwiseSum_PairwiseSum_SimpleSum
    interface Averager_PairwiseSum_PairwiseSum_SimpleSum
        module procedure a_pps_init
    end interface Averager_PairwiseSum_PairwiseSum_SimpleSum

contains

    function ps_s_init(other) result(res)
        type(SimpleSum), intent(in) :: other
        type(PairwiseSum_SimpleSum) :: res
        res%other = other
    end function ps_s_init

    recursive function ps_s_sum_i(self, x) result(s)
        class(PairwiseSum_SimpleSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: s, m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function ps_s_sum_i

    recursive function ps_s_sum_r(self, x) result(s)
        class(PairwiseSum_SimpleSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function ps_s_sum_r

    function ps_ps_init(other) result(res)
        type(PairwiseSum_SimpleSum), intent(in) :: other
        type(PairwiseSum_PairwiseSum_SimpleSum) :: res
        res%other = other
    end function ps_ps_init

    recursive function ps_ps_sum_i(self, x) result(s)
        class(PairwiseSum_PairwiseSum_SimpleSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: s, m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function ps_ps_sum_i

    recursive function ps_ps_sum_r(self, x) result(s)
        class(PairwiseSum_PairwiseSum_SimpleSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function ps_ps_sum_r

    function ps_c_init(other) result(res)
        type(ScaledSum), intent(in) :: other
        type(PairwiseSum_ScaledSum) :: res
        res%other = other
    end function ps_c_init

    recursive function ps_c_sum_i(self, x) result(s)
        class(PairwiseSum_ScaledSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: s, m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function ps_c_sum_i

    recursive function ps_c_sum_r(self, x) result(s)
        class(PairwiseSum_ScaledSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function ps_c_sum_r

    function b_s_make(item, extra) result(res)
        type(SimpleSum), intent(in) :: item
        integer, intent(in) :: extra
        type(Box_SimpleSum) :: res
        res%item = item
        res%copies = extra + 1
    end function b_s_make

    function b_s_sum_i(self, x) result(s)
        class(Box_SimpleSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: s
        s = self%copies * self%item%sum(x)
    end function b_s_sum_i

    function b_s_sum_r(self, x) result(s)
        class(Box_SimpleSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        s = real(self%copies, real64) * self%item%sum(x)
    end function b_s_sum_r

    function b_c_make(item, extra) result(res)
        type(ScaledSum), intent(in) :: item
        integer, intent(in) :: extra
        type(Box_ScaledSum) :: res
        res%item = item
        res%copies = extra + 1
    end function b_c_make

    function b_c_sum_i(self, x) result(s)
        class(Box_ScaledSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: s
        s = self%copies * self%item%sum(x)
    end function b_c_sum_i

    function b_c_sum_r(self, x) result(s)
        class(Box_ScaledSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        s = real(self%copies, real64) * self%item%sum(x)
    end function b_c_sum_r

    function ps_bc_init(other) result(res)
        type(Box_ScaledSum), intent(in) :: other
        type(PairwiseSum_Box_ScaledSum) :: res
        res%other = other
    end function ps_bc_init

    recursive function ps_bc_sum_i(self, x) result(s)
        class(PairwiseSum_Box_ScaledSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: s, m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function ps_bc_sum_i

    recursive function ps_bc_sum_r(self, x) result(s)
        class(PairwiseSum_Box_ScaledSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function ps_bc_sum_r

    function b_pc_make(item, extra) result(res)
        type(PairwiseSum_ScaledSum), intent(in) :: item
        integer, intent(in) :: extra
        type(Box_PairwiseSum_ScaledSum) :: res
        res%item = item
        res%copies = extra + 1
    end function b_pc_make

    function b_pc_sum_i(self, x) result(s)
        class(Box_PairwiseSum_ScaledSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: s
        s = self%copies * self%item%sum(x)
    end function b_pc_sum_i

    function b_pc_sum_r(self, x) result(s)
        class(Box_PairwiseSum_ScaledSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        s = real(self%copies, real64) * self%item%sum(x)
    end function b_pc_sum_r

    function a_ps_init(drv) result(res)
        type(PairwiseSum_SimpleSum), intent(in) :: drv
        type(Averager_PairwiseSum_SimpleSum) :: res
        res%drv = drv
    end function a_ps_init

    function a_ps_average_i(self, x) result(a)
        class(Averager_PairwiseSum_SimpleSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: a
        a = self%drv%sum(x) / size(x)
    end function a_ps_average_i

    function a_ps_average_r(self, x) result(a)
        class(Averager_PairwiseSum_SimpleSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: a
        a = self%drv%sum(x) / real(size(x), real64)
    end function a_ps_average_r

    function a_bc_init(drv) result(res)
        type(Box_ScaledSum), intent(in) :: drv
        type(Averager_Box_ScaledSum) :: res
        res%drv = drv
    end function a_bc_init

    function a_bc_average_i(self, x) result(a)
        class(Averager_Box_ScaledSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: a
        a = self%drv%sum(x) / size(x)
    end function a_bc_average_i

    function a_bc_average_r(self, x) result(a)
        class(Averager_Box_ScaledSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: a
        a = self%drv%sum(x) / real(size(x), real64)
    end function a_bc_average_r

    function a_bs_init(drv) result(res)
        type(Box_SimpleSum), intent(in) :: drv
        type(Averager_Box_SimpleSum) :: res
        res%drv = drv
    end function a_bs_init

    function a_bs_average_i(self, x) result(a)
        class(Averager_Box_SimpleSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: a
        a = self%drv%sum(x) / size(x)
    end function a_bs_average_i

    function a_bs_average_r(self, x) result(a)
        class(Averager_Box_SimpleSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: a
        a = self%drv%sum(x) / real(size(x), real64)
    end function a_bs_average_r

    function a_pps_init(drv) result(res)
        type(PairwiseSum_PairwiseSum_SimpleSum), intent(in) :: drv
        type(Averager_PairwiseSum_PairwiseSum_SimpleSum) :: res
        res%drv = drv
    end function a_pps_init

    function a_pps_average_i(self, x) result(a)
        class(Averager_PairwiseSum_PairwiseSum_SimpleSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: a
        a = self%drv%sum(x) / size(x)
    end function a_pps_average_i

    function a_pps_average_r(self, x) result(a)
        class(Averager_PairwiseSum_PairwiseSum_SimpleSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: a
        a = self%drv%sum(x) / real(size(x), real64)
    end function a_pps_average_r
end module traits_generic_type_01_oracle_instances

! The renamed re-export of the generic version names the same concrete type.
module traits_generic_type_01_oracle_facade
    use traits_generic_type_01_oracle_instances, only: &
        Pair_SimpleSum => PairwiseSum_SimpleSum
    implicit none
    private
    public :: Pair_SimpleSum
end module traits_generic_type_01_oracle_facade

program traits_generic_type_01_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_type_01_oracle_averager, only: IAverager
    use traits_generic_type_01_oracle_leaves, only: SimpleSum, ScaledSum
    use traits_generic_type_01_oracle_instances
    use traits_generic_type_01_oracle_facade, only: Pair_SimpleSum
    implicit none

    type(PairwiseSum_SimpleSum) :: plain, explicit
    type(Pair_SimpleSum) :: renamed
    type(PairwiseSum_PairwiseSum_SimpleSum) :: nested
    type(Box_ScaledSum) :: boxed
    type(PairwiseSum_ScaledSum) :: scaled
    type(Box_SimpleSum) :: box1, box4
    type(Box_ScaledSum) :: box5
    type(PairwiseSum_Box_ScaledSum) :: pboxed
    type(Box_PairwiseSum_ScaledSum) :: boxpair
    class(IAverager), allocatable :: av
    integer, allocatable :: xi(:)
    real(real64), allocatable :: xr(:)
    integer :: n, i, base, checks

    checks = 0
    plain = PairwiseSum_SimpleSum(SimpleSum())
    explicit = PairwiseSum_SimpleSum(other = SimpleSum())
    plain = explicit
    renamed = plain
    nested = PairwiseSum_PairwiseSum_SimpleSum(PairwiseSum_SimpleSum(SimpleSum()))
    boxed = Box_ScaledSum(item = ScaledSum(2), copies = 5)

    scaled = PairwiseSum_ScaledSum(ScaledSum(3))
    box1 = Box_SimpleSum(SimpleSum())
    box4 = Box_SimpleSum(SimpleSum(), 3)
    box5 = Box_ScaledSum(ScaledSum(5))
    pboxed = PairwiseSum_Box_ScaledSum(Box_ScaledSum(ScaledSum(2), 2))
    boxpair = Box_PairwiseSum_ScaledSum(PairwiseSum_ScaledSum(ScaledSum(4)))

    do n = 0, 9
        xi = [(i, i = 1, n)]
        xr = [(real(i, real64), i = 1, n)]
        base = n * (n + 1) / 2
        call check_i(plain%sum(xi), base, 1)
        call check_r(plain%sum(xr), base, 2)
        call check_i(renamed%sum(xi), base, 3)
        call check_i(total_of(renamed, xi), base, 4)
        call check_i(nested%sum(xi), base, 5)
        call check_r(nested%sum(xr), base, 6)
        call check_i(scaled%sum(xi), 3 * base, 7)
        call check_r(scaled%sum(xr), 3 * base, 8)
        call check_i(box1%sum(xi), base, 9)
        call check_i(box4%sum(xi), 4 * base, 10)
        call check_i(boxed%sum(xi), 10 * base, 11)
        call check_r(boxed%sum(xr), 10 * base, 12)
        call check_i(box5%sum(xi), 5 * base, 13)
        call check_i(pboxed%sum(xi), 6 * base, 14)
        call check_r(pboxed%sum(xr), 6 * base, 15)
        call check_i(boxpair%sum(xi), 4 * base, 16)
    end do

    xi = [1, 2, 3, 4, 5]
    xr = [1.d0, 2.d0, 3.d0, 4.d0, 5.d0]
    av = Averager_PairwiseSum_SimpleSum(drv = PairwiseSum_SimpleSum(other = SimpleSum()))
    call check_i(av%average(xi), 3, 20)
    call check_mean(av%average(xr), 3.d0, 21)
    av = Averager_Box_ScaledSum(drv = boxed)
    call check_i(av%average(xi), 30, 22)
    call check_mean(av%average(xr), 30.d0, 23)
    av = Averager_Box_SimpleSum(Box_SimpleSum(SimpleSum(), 1))
    call check_i(av%average(xi), 6, 24)
    call check_mean(av%average(xr), 6.d0, 25)
    av = Averager_PairwiseSum_PairwiseSum_SimpleSum(drv = nested)
    call check_i(av%average(xi), 3, 26)
    call check_mean(av%average(xr), 3.d0, 27)

    if (checks /= 168) error stop 99
    print '(a,i0)', 'traits_generic_type_01 checks: ', checks

contains

    integer function total_of(p, x) result(r)
        type(PairwiseSum_SimpleSum), intent(in) :: p
        integer, intent(in) :: x(:)
        r = p%sum(x)
    end function total_of

    subroutine check_i(actual, expected, code)
        integer, intent(in) :: actual, expected, code
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,a,i0,a,i0)', 'check ', code, ': ', actual, ' /= ', expected
            error stop code
        end if
    end subroutine check_i

    subroutine check_r(actual, expected, code)
        real(real64), intent(in) :: actual
        integer, intent(in) :: expected, code
        checks = checks + 1
        if (actual /= real(expected, real64)) then
            print '(a,i0,a,g0,a,i0)', 'check ', code, ': ', actual, ' /= ', expected
            error stop code
        end if
    end subroutine check_r

    subroutine check_mean(actual, expected, code)
        real(real64), intent(in) :: actual, expected
        integer, intent(in) :: code
        checks = checks + 1
        if (abs(actual - expected) > 4 * spacing(expected)) then
            print '(a,i0,a,g0,a,g0)', 'check ', code, ': ', actual, ' /= ', expected
            error stop code
        end if
    end subroutine check_mean

end program traits_generic_type_01_oracle
