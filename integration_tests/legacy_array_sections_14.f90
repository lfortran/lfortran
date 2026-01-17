! Stress test for stack usage when legacy sequence association rewrites a(i)
! to an array section for an external (separate-compilation) assumed-size dummy.
!
! The key is to avoid materializing array-section descriptors when only the
! address of the first element is required.
program legacy_array_sections_14
    use, intrinsic :: iso_c_binding, only: c_int, c_long
    implicit none

    integer, parameter :: n = 300
    integer, parameter :: depth = 25
    integer, parameter :: check_idx = 150

    real, allocatable :: a(:)

    call limit_stack_bytes(262144_c_long)

    allocate(a(n))
    a = 0.0

    call recurse(depth, a)

    if (abs(a(check_idx) - real(depth)) > 1.0e-6) error stop

    print *, "PASSED"

contains

    subroutine limit_stack_bytes(bytes)
        use, intrinsic :: iso_c_binding, only: c_int, c_long
        implicit none

        integer(c_long), intent(in) :: bytes

        integer(c_int), parameter :: rlimit_stack = 3

        type, bind(c) :: rlimit_t
            integer(c_long) :: rlim_cur
            integer(c_long) :: rlim_max
        end type rlimit_t

        interface
            integer(c_int) function setrlimit(resource, rl) bind(c, name="setrlimit")
                import :: c_int, rlimit_t
                integer(c_int), value :: resource
                type(rlimit_t), intent(in) :: rl
            end function setrlimit
        end interface

        type(rlimit_t) :: rl
        integer(c_int) :: rc

        rl%rlim_cur = bytes
        rl%rlim_max = bytes
        rc = setrlimit(rlimit_stack, rl)
        if (rc /= 0) error stop
    end subroutine limit_stack_bytes

    recursive subroutine recurse(level, a)
        implicit none

        integer, intent(in) :: level
        real, intent(inout) :: a(*)

        external :: foo

        if (level == 0) return

        ! Many distinct call sites to amplify per-function stack usage.
        call foo(a(1))
        call foo(a(2))
        call foo(a(3))
        call foo(a(4))
        call foo(a(5))
        call foo(a(6))
        call foo(a(7))
        call foo(a(8))
        call foo(a(9))
        call foo(a(10))
        call foo(a(11))
        call foo(a(12))
        call foo(a(13))
        call foo(a(14))
        call foo(a(15))
        call foo(a(16))
        call foo(a(17))
        call foo(a(18))
        call foo(a(19))
        call foo(a(20))
        call foo(a(21))
        call foo(a(22))
        call foo(a(23))
        call foo(a(24))
        call foo(a(25))
        call foo(a(26))
        call foo(a(27))
        call foo(a(28))
        call foo(a(29))
        call foo(a(30))
        call foo(a(31))
        call foo(a(32))
        call foo(a(33))
        call foo(a(34))
        call foo(a(35))
        call foo(a(36))
        call foo(a(37))
        call foo(a(38))
        call foo(a(39))
        call foo(a(40))
        call foo(a(41))
        call foo(a(42))
        call foo(a(43))
        call foo(a(44))
        call foo(a(45))
        call foo(a(46))
        call foo(a(47))
        call foo(a(48))
        call foo(a(49))
        call foo(a(50))
        call foo(a(51))
        call foo(a(52))
        call foo(a(53))
        call foo(a(54))
        call foo(a(55))
        call foo(a(56))
        call foo(a(57))
        call foo(a(58))
        call foo(a(59))
        call foo(a(60))
        call foo(a(61))
        call foo(a(62))
        call foo(a(63))
        call foo(a(64))
        call foo(a(65))
        call foo(a(66))
        call foo(a(67))
        call foo(a(68))
        call foo(a(69))
        call foo(a(70))
        call foo(a(71))
        call foo(a(72))
        call foo(a(73))
        call foo(a(74))
        call foo(a(75))
        call foo(a(76))
        call foo(a(77))
        call foo(a(78))
        call foo(a(79))
        call foo(a(80))
        call foo(a(81))
        call foo(a(82))
        call foo(a(83))
        call foo(a(84))
        call foo(a(85))
        call foo(a(86))
        call foo(a(87))
        call foo(a(88))
        call foo(a(89))
        call foo(a(90))
        call foo(a(91))
        call foo(a(92))
        call foo(a(93))
        call foo(a(94))
        call foo(a(95))
        call foo(a(96))
        call foo(a(97))
        call foo(a(98))
        call foo(a(99))
        call foo(a(100))
        call foo(a(101))
        call foo(a(102))
        call foo(a(103))
        call foo(a(104))
        call foo(a(105))
        call foo(a(106))
        call foo(a(107))
        call foo(a(108))
        call foo(a(109))
        call foo(a(110))
        call foo(a(111))
        call foo(a(112))
        call foo(a(113))
        call foo(a(114))
        call foo(a(115))
        call foo(a(116))
        call foo(a(117))
        call foo(a(118))
        call foo(a(119))
        call foo(a(120))
        call foo(a(121))
        call foo(a(122))
        call foo(a(123))
        call foo(a(124))
        call foo(a(125))
        call foo(a(126))
        call foo(a(127))
        call foo(a(128))
        call foo(a(129))
        call foo(a(130))
        call foo(a(131))
        call foo(a(132))
        call foo(a(133))
        call foo(a(134))
        call foo(a(135))
        call foo(a(136))
        call foo(a(137))
        call foo(a(138))
        call foo(a(139))
        call foo(a(140))
        call foo(a(141))
        call foo(a(142))
        call foo(a(143))
        call foo(a(144))
        call foo(a(145))
        call foo(a(146))
        call foo(a(147))
        call foo(a(148))
        call foo(a(149))
        call foo(a(150))
        call foo(a(151))
        call foo(a(152))
        call foo(a(153))
        call foo(a(154))
        call foo(a(155))
        call foo(a(156))
        call foo(a(157))
        call foo(a(158))
        call foo(a(159))
        call foo(a(160))
        call foo(a(161))
        call foo(a(162))
        call foo(a(163))
        call foo(a(164))
        call foo(a(165))
        call foo(a(166))
        call foo(a(167))
        call foo(a(168))
        call foo(a(169))
        call foo(a(170))
        call foo(a(171))
        call foo(a(172))
        call foo(a(173))
        call foo(a(174))
        call foo(a(175))
        call foo(a(176))
        call foo(a(177))
        call foo(a(178))
        call foo(a(179))
        call foo(a(180))
        call foo(a(181))
        call foo(a(182))
        call foo(a(183))
        call foo(a(184))
        call foo(a(185))
        call foo(a(186))
        call foo(a(187))
        call foo(a(188))
        call foo(a(189))
        call foo(a(190))
        call foo(a(191))
        call foo(a(192))
        call foo(a(193))
        call foo(a(194))
        call foo(a(195))
        call foo(a(196))
        call foo(a(197))
        call foo(a(198))
        call foo(a(199))
        call foo(a(200))

        call recurse(level - 1, a)
    end subroutine recurse

end program legacy_array_sections_14
