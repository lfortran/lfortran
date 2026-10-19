! A scoping unit that references an external procedure defined in the same
! file as a procedure (here with a CALL statement) accesses it everywhere in
! that unit: in its specification part, as an actual argument, and in the
! procedures it contains, by host association. A procedure with ENTRY
! statements accesses its own name and its ENTRY names.
subroutine dist()
    integer :: ncalls
    common /counter/ ncalls
    ncalls = ncalls + 1
end subroutine

subroutine bar(f)
    interface
        subroutine f()
        end subroutine
    end interface
    call f()
end subroutine

subroutine init_target()
    interface
        subroutine iface()
        end subroutine
    end interface
    procedure(iface), pointer, save :: p => dist
    call dist()
    call p()
end subroutine

subroutine call_and_pass()
    interface
        subroutine bar(f)
            interface
                subroutine f()
                end subroutine
            end interface
        end subroutine
    end interface
    call dist()
    call bar(dist)
end subroutine

subroutine host()
    interface
        subroutine bar(f)
            interface
                subroutine f()
                end subroutine
            end interface
        end subroutine
        subroutine iface()
        end subroutine
    end interface
    procedure(iface), pointer :: p
    call dist()
    call inner()
    call inner_init()
contains
    subroutine inner()
        call bar(dist)
        p => dist
        if (.not. associated(p, dist)) error stop
    end subroutine
    subroutine inner_init()
        procedure(iface), pointer, save :: q => dist
        call q()
    end subroutine
end subroutine

integer function with_entry(n)
    integer :: n, other_entry
    interface
        subroutine bar(f)
            interface
                subroutine f()
                end subroutine
            end interface
        end subroutine
    end interface
    call dist()
    call bar(dist)
    with_entry = n
    return
entry other_entry(n)
    call bar(dist)
    other_entry = 2*n
end function

subroutine with_block()
    interface
        subroutine bar(f)
            interface
                subroutine f()
                end subroutine
            end interface
        end subroutine
    end interface
    call dist()
    block
        call bar(dist)
    end block
end subroutine

subroutine bar_n(f, n)
    integer :: n
    interface
        subroutine f(n)
            integer :: n
        end subroutine
    end interface
    call f(n)
end subroutine

recursive subroutine self_entry(n)
    integer :: n
    integer :: ncalls
    common /counter/ ncalls
    interface
        subroutine bar_n(f, n)
            integer :: n
            interface
                subroutine f(n)
                    integer :: n
                end subroutine
            end interface
        end subroutine
    end interface
    ncalls = ncalls + 1
    if (n > 0) call bar_n(self_entry, n - 1)
    return
entry self_entry2(n)
    ncalls = ncalls + 10
    if (n > 0) call bar_n(self_entry2, n - 1)
end subroutine

integer function apply(f, n)
    integer :: n
    interface
        integer function f(n)
            integer :: n
        end function
    end interface
    apply = f(n)
end function

recursive integer function self_entry_fn(n) result(r)
    integer :: n, r2
    interface
        integer function apply(f, n)
            integer :: n
            interface
                integer function f(n)
                    integer :: n
                end function
            end interface
        end function
    end interface
    r = 1
    if (n > 0) r = 10 + apply(self_entry_fn, n - 1)
    return
entry self_entry_fn2(n) result(r2)
    r2 = 2
    if (n > 0) r2 = 20 + apply(self_entry_fn2, n - 1)
end function

program external_31
    integer :: ncalls
    common /counter/ ncalls
    interface
        integer function with_entry(n)
            integer :: n
        end function
        integer function other_entry(n)
            integer :: n
        end function
        integer function self_entry_fn(n)
            integer :: n
        end function
        integer function self_entry_fn2(n)
            integer :: n
        end function
    end interface
    ncalls = 0
    call init_target()
    if (ncalls /= 2) error stop
    call call_and_pass()
    if (ncalls /= 4) error stop
    call host()
    if (ncalls /= 7) error stop
    if (with_entry(3) /= 3) error stop
    if (ncalls /= 9) error stop
    if (other_entry(3) /= 6) error stop
    if (ncalls /= 10) error stop
    call with_block()
    if (ncalls /= 12) error stop
    call self_entry(1)
    if (ncalls /= 14) error stop
    call self_entry2(1)
    if (ncalls /= 34) error stop
    if (self_entry_fn(1) /= 11) error stop
    if (self_entry_fn2(1) /= 22) error stop
    print *, ncalls
end program
