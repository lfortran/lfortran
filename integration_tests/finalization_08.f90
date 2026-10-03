! A nonpointer function result of a finalizable type is finalized after the
! statement that references the function (F2018 7.5.6.3 p5), and not when the
! function is invoked. The variable of an intrinsic assignment is finalized
! before it is defined, unless it is an unallocated allocatable (p1).
!
! gfortran does not finalize some of these results (those of a function
! referenced by an expression that is not the whole right-hand side of an
! assignment). For it, the checks marked "strict" also accept that nothing was
! finalized; for any other compiler they require the finalizations the
! standard requires.
module finalization_08_m
    use iso_fortran_env, only: compiler_version
    implicit none
    logical :: strict = .true.
    integer :: nfin = 0
    integer :: fin_log(20) = -100
    integer :: nfin_on_entry = -1
    integer :: nfin_in_call = -1
    integer :: nfreed_in_call = -1
    integer :: ncalls = 0
    type :: t
        integer :: c = 0
        integer :: d = -1
        integer, allocatable :: v(:)
    contains
        final :: fin
    end type
    ! Its final subroutine releases what the component points to, so that
    ! a copy used or finalized after the result is finalized goes wrong.
    type :: h
        integer, pointer :: p => null()
    contains
        final :: fin_h
    end type
    ! The same with an elemental final subroutine, which also finalizes an
    ! array of this type, element by element (F2018 7.5.6.2).
    type :: e
        integer, pointer :: p => null()
    contains
        final :: fin_e
    end type
contains
    function construct(c) result(r)
        integer, intent(in) :: c
        type(t) :: r
        nfin_on_entry = nfin
        ncalls = ncalls + 1
        if (r%d /= -1) error stop "result not default-initialized"
        if (allocated(r%v)) error stop "result component already allocated"
        r%c = c
        allocate(r%v(2))
        r%v = c
    end function

    function construct_h(c) result(r)
        integer, intent(in) :: c
        type(h) :: r
        ncalls = ncalls + 1
        if (associated(r%p)) error stop "result pointer not default-initialized"
        allocate(r%p)
        r%p = c
    end function

    subroutine fin_h(self)
        type(h), intent(inout) :: self
        nfin = nfin + 1
        if (associated(self%p)) then
            fin_log(nfin) = self%p
            deallocate(self%p)
        end if
    end subroutine

    impure elemental subroutine fin_e(self)
        type(e), intent(inout) :: self
        nfin = nfin + 1
        if (associated(self%p)) then
            fin_log(nfin) = self%p
            deallocate(self%p)
        end if
    end subroutine

    elemental function construct_he(c) result(r)
        integer, intent(in) :: c
        type(h) :: r
        allocate(r%p)
        r%p = c
    end function

    elemental function construct_e(c) result(r)
        integer, intent(in) :: c
        type(e) :: r
        allocate(r%p)
        r%p = c
    end function

    elemental integer function value_h(x)
        type(h), intent(in) :: x
        value_h = x%p
    end function

    elemental integer function value_e(x)
        type(e), intent(in) :: x
        value_e = x%p
    end function

    ! The results freed since reset(): those finalized while defined.
    integer function nfreed()
        nfreed = count(fin_log(1:nfin) /= -100)
    end function

    subroutine show_h(x, expected)
        type(h), intent(in) :: x(:)
        integer, intent(in) :: expected(:)
        integer :: j
        if (size(x) /= size(expected)) error stop "show_h: wrong size"
        do j = 1, size(x)
            if (.not. associated(x(j)%p)) error stop "show_h: result freed"
            if (x(j)%p /= expected(j)) error stop "show_h: wrong value"
        end do
        nfreed_in_call = nfreed()
    end subroutine

    subroutine show_e(x, expected)
        type(e), intent(in) :: x(:)
        integer, intent(in) :: expected(:)
        integer :: j
        if (size(x) /= size(expected)) error stop "show_e: wrong size"
        do j = 1, size(x)
            if (.not. associated(x(j)%p)) error stop "show_e: result freed"
            if (x(j)%p /= expected(j)) error stop "show_e: wrong value"
        end do
        nfreed_in_call = nfreed()
    end subroutine

    ! Strict: the results freed since reset() are those with the values
    ! `expected`, in any order. The final subroutine may also have been
    ! called for an object that was never defined (see temporary_results).
    subroutine expect_freed(expected, what)
        integer, intent(in) :: expected(:)
        character(*), intent(in) :: what
        integer :: freed(20), n, j
        n = 0
        do j = 1, nfin
            if (fin_log(j) /= -100) then
                n = n + 1
                freed(n) = fin_log(j)
            end if
        end do
        if (n == 0 .and. .not. strict) return
        if (n /= size(expected)) then
            print *, what, ": freed", freed(1:n), "expected", expected
            error stop "wrong number of freed results"
        end if
        do j = 1, n
            if (count(freed(1:n) == expected(j)) /= &
                    count(expected == expected(j))) then
                print *, what, ": freed", freed(1:n), "expected", expected
                error stop "freed entities are not the results"
            end if
        end do
    end subroutine

    elemental integer function add_p(a, x)
        integer, intent(in) :: a
        type(h), intent(in) :: x
        add_p = a + x%p
    end function

    integer function comp(x)
        type(t), intent(in) :: x
        comp = x%c
    end function

    subroutine fin(self)
        type(t), intent(inout) :: self
        nfin = nfin + 1
        fin_log(nfin) = self%c
    end subroutine

    subroutine reset()
        nfin = 0
        fin_log = -100
        ncalls = 0
    end subroutine

    ! Strict: the results finalized since reset() are those with the values
    ! `expected`, in any order.
    subroutine expect_results(expected, what)
        integer, intent(in) :: expected(:)
        character(*), intent(in) :: what
        integer :: j
        if (nfin == 0 .and. .not. strict) return
        if (nfin /= size(expected)) then
            print *, what, ": finalized", nfin, "results, expected", size(expected)
            error stop "wrong number of finalized results"
        end if
        do j = 1, size(expected)
            if (count(fin_log(1:nfin) == expected(j)) /= &
                    count(expected == expected(j))) then
                print *, what, ": finalized", fin_log(1:nfin)
                error stop "finalized entities are not the results"
            end if
        end do
    end subroutine

    elemental integer function add_c(a, x)
        integer, intent(in) :: a
        type(t), intent(in) :: x
        add_c = a + x%c
    end function

    elemental subroutine add_c_sub(a, x)
        integer, intent(inout) :: a
        type(t), intent(in) :: x
        a = a + x%c
    end subroutine

    subroutine use_value(x, expected)
        type(t), intent(in) :: x
        integer, intent(in) :: expected
        if (x%c /= expected) error stop "wrong actual argument"
        nfin_in_call = nfin
    end subroutine

    subroutine only_temporaries()
        integer :: k
        k = comp(construct(6))
        if (k /= 6) error stop
        call use_value(construct(12), 12)
    end subroutine

    ! The associate name is the result: it is finalized once, after the
    ! construct, and it is not a copy of an undefined object.
    subroutine associate_result()
        call reset()
        associate (a => construct(20))
            if (a%c /= 20 .or. any(a%v /= 20)) error stop "associate value"
        end associate
        if (ncalls /= 1) error stop "associate: selector evaluated again"
        if (nfin /= 1) error stop "associate: expected 1 finalization"
        if (fin_log(1) /= 20) error stop "associate: finalized entity is not the result"
    end subroutine

    ! Sourced allocation defines the new object from the result, which is
    ! finalized after the statement; nothing undefined is finalized.
    subroutine allocate_source_result()
        type(t), allocatable :: y, ya(:)
        call reset()
        allocate(y, source=construct(21))
        if (ncalls /= 1) error stop "allocate: source evaluated again"
        call expect_results([21], "allocate")
        if (y%c /= 21 .or. any(y%v /= 21)) error stop "allocate value"
        ! Each element of an array object gets the value of the source,
        ! which is evaluated once.
        call reset()
        allocate(ya(3), source=construct(22))
        if (ncalls /= 1) error stop "allocate array: source evaluated again"
        if (any(ya%c /= 22)) error stop "allocate array value"
        if (any(ya(2)%v /= 22)) error stop "allocate array component value"
    end subroutine

    ! A result referenced by the header of a construct, or by a statement
    ! of any other kind, is finalized for each execution, and not only once
    ! when the procedure returns.
    subroutine header_results()
        integer :: i, k, m(3)
        character(len=4) :: buf(3)
        call reset()
        do i = 1, 5
            if (comp(construct(i)) > 100) error stop "if value"
        end do
        if (ncalls /= 5) error stop "if: wrong number of evaluations"
        call expect_results([1, 2, 3, 4, 5], "if")
        call reset()
        k = 0
        do while (comp(construct(k)) < 4)
            k = k + 1
        end do
        if (ncalls /= 5) error stop "do while: wrong number of evaluations"
        call expect_results([0, 1, 2, 3, 4], "do while")
        call reset()
        do i = 1, 3
            do k = comp(construct(1)), comp(construct(2))
            end do
        end do
        if (ncalls /= 6) error stop "do: wrong number of evaluations"
        call expect_results([1, 2, 1, 2, 1, 2], "do")
        call reset()
        buf = ['7', '8', '9']
        do i = 1, 3
            read(buf(comp(construct(i))), *) m(i)
        end do
        if (any(m /= [7, 8, 9])) error stop "read value"
        if (ncalls /= 3) error stop "read: wrong number of evaluations"
        call expect_results([1, 2, 3], "read")
        call reset()
        k = 0
        do i = 1, 3
            if (comp(construct(i)) - 2) 10, 20, 30
10          k = k + 1
20          k = k + 10
30          k = k + 100
        end do
        if (k /= 321) error stop "arithmetic if value"
        if (ncalls /= 3) error stop "arithmetic if: wrong number of evaluations"
        call expect_results([1, 2, 3], "arithmetic if")
        call reset()
    end subroutine

    ! A scalar operand of an array expression, or a scalar actual argument
    ! of an elemental procedure referenced with an array argument, is
    ! evaluated once, not once per element.
    subroutine array_operand_results()
        integer :: b(3), v(3)
        call reset()
        b = [1, 2, 3]
        v = b + comp(construct(1))
        if (any(v /= [2, 3, 4])) error stop "array expression value"
        if (ncalls /= 1) error stop "array expression: evaluated per element"
        call expect_results([1], "array expression")
        call reset()
        where (b > comp(construct(1))) b = 0
        if (any(b /= [1, 0, 0])) error stop "where value"
        if (ncalls /= 1) error stop "where: evaluated per element"
        call expect_results([1], "where")
        call reset()
        v = comp(construct(2))
        if (any(v /= 2)) error stop "broadcast value"
        if (ncalls /= 1) error stop "broadcast: evaluated per element"
        call expect_results([2], "broadcast")
        call reset()
        b = [1, 2, 3]
        v = add_c(b, construct(5))
        if (any(v /= [6, 7, 8])) error stop "elemental function value"
        if (ncalls /= 1) error stop "elemental function: evaluated per element"
        call expect_results([5], "elemental function")
        call reset()
        v = add_c(b, construct(comp(construct(3))))
        if (any(v /= [4, 5, 6])) error stop "nested elemental value"
        if (ncalls /= 2) error stop "nested elemental: evaluated per element"
        call expect_results([3, 3], "nested elemental")
        call reset()
        call add_c_sub(b, construct(10))
        if (any(b /= [11, 12, 13])) error stop "elemental subroutine value"
        if (ncalls /= 1) error stop "elemental subroutine: evaluated per element"
        call expect_results([10], "elemental subroutine")
    end subroutine

    ! In a WHERE construct, the result is finalized after the construct,
    ! which uses the result itself and not a copy of it.
    subroutine where_elemental_results()
        integer :: b(3), v(3), i
        b = [1, 2, 3]
        v = 0
        call reset()
        where (b > 1) v = add_p(b, construct_h(5))
        if (any(v /= [0, 7, 8])) error stop "where elemental value"
        if (ncalls /= 1) error stop "where elemental: evaluated per element"
        call expect_results([5], "where elemental")
        call reset()
        v = 0
        do i = 1, 3
            where (add_p(b, construct_h(i)) > 4)
                v = v + add_p(b, construct_h(10*i))
            end where
        end do
        if (any(v /= [0, 32, 56])) error stop "where mask value"
        if (ncalls /= 6) error stop "where mask: wrong number of evaluations"
        call expect_results([1, 10, 2, 20, 3, 30], "where mask")
    end subroutine

    ! A result that a compiler temporary holds, the element of an array
    ! constructor or the value of an elemental reference with an array
    ! argument, is used by the statement and finalized after it, not before:
    ! the final subroutine frees what the component points to. The value of
    ! the elemental reference is an array, which is finalized only by an
    ! elemental final subroutine (F2018 7.5.6.2). The final subroutine is
    ! also called for the elements of an array constructor temporary before
    ! they are defined, which leaves their pointers unassociated: those
    ! calls do not free a result.
    subroutine temporary_results()
        integer :: k
        call reset()
        call show_h([construct_h(1), construct_h(2), construct_h(3)], [1, 2, 3])
        if (nfreed_in_call /= 0) error stop "array constructor: freed before the call"
        if (ncalls /= 3) error stop "array constructor: wrong number of evaluations"
        call expect_freed([1, 2, 3], "array constructor")
        call reset()
        call show_h(construct_he([4, 5, 6]), [4, 5, 6])
        if (nfreed_in_call /= 0) error stop "elemental: freed before the call"
        if (nfreed() /= 0) error stop "elemental: array result finalized"
        call reset()
        k = sum(value_h(construct_he([1, 2, 3])))
        if (k /= 6) error stop "elemental operand value"
        if (nfreed() /= 0) error stop "elemental operand: array result finalized"
        call reset()
        call show_e(construct_e([7, 8, 9]), [7, 8, 9])
        if (nfreed_in_call /= 0) error stop "elemental final: freed before the call"
        call expect_freed([7, 8, 9], "elemental final")
        call reset()
        k = sum(value_e(construct_e([1, 2, 3])))
        if (k /= 6) error stop "elemental final operand value"
        call expect_freed([1, 2, 3], "elemental final operand")
        call reset()
    end subroutine
end module

program finalization_08
    use finalization_08_m
    implicit none
    type(t), allocatable :: a, a2
    type(t) :: b, arr(2)
    integer :: k, i

    strict = index(compiler_version(), "GCC") == 0

    ! Unallocated allocatable variable: only the result is finalized.
    call reset()
    a = construct(3)
    if (nfin_on_entry /= 0) error stop "result finalized on entry"
    if (nfin /= 1) error stop "unallocated variable: expected 1 finalization"
    if (fin_log(1) /= 3) error stop "finalized entity is not the result"
    if (a%c /= 3 .or. a%d /= -1 .or. any(a%v /= 3)) error stop

    ! Allocated allocatable variable: the variable, then the result.
    call reset()
    a = construct(4)
    if (nfin_on_entry /= 0) error stop "result finalized on entry"
    if (nfin /= 2) error stop "allocated variable: expected 2 finalizations"
    if (fin_log(1) /= 3 .or. fin_log(2) /= 4) error stop
    if (a%c /= 4 .or. any(a%v /= 4)) error stop

    ! Allocation defines the new object; it does not finalize it.
    b%c = 4
    call reset()
    allocate(a2, source=b)
    if (nfin /= 0) error stop "sourced allocation finalized the new object"
    if (a2%c /= 4) error stop

    ! Nonallocatable variable: the variable, then the result.
    b%c = 7
    call reset()
    b = construct(5)
    if (nfin /= 2) error stop "variable: expected 2 finalizations"
    if (fin_log(1) /= 7 .or. fin_log(2) /= 5) error stop
    if (b%c /= 5) error stop

    call reset()
    arr(2)%c = 1
    arr(2) = construct(8)
    if (nfin /= 2) error stop "array element: expected 2 finalizations"
    if (fin_log(1) /= 1 .or. fin_log(2) /= 8) error stop
    if (arr(2)%c /= 8) error stop

    ! A reference in an expression: the result is finalized once the
    ! statement is done, every time it is executed.
    call reset()
    k = comp(construct(6))
    if (k /= 6) error stop
    if (nfin_on_entry /= 0) error stop "result finalized on entry"
    if (nfin /= 1) error stop "expression: expected 1 finalization"
    if (fin_log(1) /= 6) error stop

    call reset()
    do i = 1, 3
        b%c = comp(construct(10 + i))
        if (nfin /= i) error stop "loop: result not finalized after statement"
        if (fin_log(i) /= 10 + i) error stop
    end do
    if (b%c /= 13) error stop

    call reset()
    call use_value(construct(12), 12)
    if (nfin_in_call /= 0) error stop "result finalized before the call"
    if (nfin /= 1) error stop "actual argument: expected 1 finalization"
    if (fin_log(1) /= 12) error stop

    ! The results are not finalized again when the procedure returns.
    call reset()
    call only_temporaries()
    if (nfin /= 2) error stop "results finalized again at the end"
    if (fin_log(1) /= 6 .or. fin_log(2) /= 12) error stop

    ! A result the condition of an IF construct references, over and over:
    ! each invocation starts from a default-initialized result.
    call reset()
    do i = 1, 3
        if (comp(construct(i)) /= i) error stop
    end do
    call expect_results([1, 2, 3], "program if")

    call associate_result()
    call allocate_source_result()
    call header_results()
    if (nfin /= 0) error stop "header results finalized again at the end"
    call array_operand_results()
    call where_elemental_results()
    call temporary_results()
    print *, "ok"
end program
