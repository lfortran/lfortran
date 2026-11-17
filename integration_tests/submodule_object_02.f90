! The object file of a submodule defines its procedures, which may use what
! the program's object file does not define: a module only the submodule
! uses, or a procedure of the parent module that the program does not call.
! The program's object file may define the submodule's variables too, once
! the submodule has been read. Each build order has to link and share one
! copy of every variable.
program submodule_object_02
    implicit none
    character(len=1024) :: compiler
    character(len=8) :: opt
    integer :: length, status
    call get_environment_variable("LFORTRAN", compiler, length, status)
    if (status /= 0 .or. length <= 0) compiler = "lfortran"
    opt = optimization_flag(compiler)
    call write_sources()
    call compile(compiler, "u")
    call compile(compiler, "m")
    ! The program is compiled before the submodules have been read.
    call run(compiler, "-c submodule_object_02_p.f90 -o submodule_object_02_p_first.o", 1)
    call compile(compiler, "s")
    call compile(compiler, "t")
    call check_unused_dependency(compiler, opt)
    call link_and_run(compiler, "submodule_object_02_p_first.o", "submodule_object_02_bin1", 2)
    ! Now the program reads the submodules and compiles them too.
    call run(compiler, "-c submodule_object_02_p.f90 -o submodule_object_02_p_last.o", 4)
    call link_and_run(compiler, "submodule_object_02_p_last.o", "submodule_object_02_bin3", 5)
    ! An optimized copy must use the SAVE storage of the selected procedure.
    call compile_with_flags(compiler, opt, "save_m")
    call compile_with_flags(compiler, opt, "save_s")
    call compile_with_flags(compiler, opt, "save_p")
    call compile_with_flags(compiler, opt, "save_x")
    call run(compiler, trim(opt) // " submodule_object_02_save_p.o " // &
        "submodule_object_02_save_m.o submodule_object_02_save_s.o " // &
        "submodule_object_02_save_x.o -o submodule_object_02_save_bin", 8)
    call run_binary("submodule_object_02_save_bin", 9)
contains
    function optimization_flag(compiler) result(flag)
        character(len=*), intent(in) :: compiler
        character(len=8) :: flag
        character(len=256) :: line
        integer :: ios
        call run(compiler, "--version > submodule_object_02_version.txt 2>&1", 10)
        open(unit=11, file="submodule_object_02_version.txt", status="old")
        read(11, '(a)', iostat=ios) line
        close(11)
        flag = "-O2"
        if (ios == 0 .and. index(line, "LFortran") /= 0) flag = "--fast"
    end function optimization_flag
    subroutine check_unused_dependency(compiler, flag)
        character(len=*), intent(in) :: compiler, flag
        character(len=4096) :: line
        integer :: ios
        if (trim(flag) /= "--fast") return
        call run(compiler, "--show-llvm submodule_object_02_s.f90 > " // &
            "submodule_object_02_s.ll", 10)
        open(unit=11, file="submodule_object_02_s.ll", status="old")
        do
            read(11, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, "__module_submodule_object_02_u_unused") /= 0) then
                close(11)
                error stop 11
            end if
        end do
        close(11)
    end subroutine check_unused_dependency
    subroutine write_sources()
        open(unit=10, file="submodule_object_02_u.f90", status="replace")
        write(10, '(a)') "module submodule_object_02_u"
        write(10, '(a)') "integer :: offset = 1"
        write(10, '(a)') "contains"
        write(10, '(a)') "integer function bump(n)"
        write(10, '(a)') "integer, intent(in) :: n"
        write(10, '(a)') "bump = n + offset"
        write(10, '(a)') "end function bump"
        write(10, '(a)') "integer function unused()"
        write(10, '(a)') "unused = 99"
        write(10, '(a)') "end function unused"
        write(10, '(a)') "end module submodule_object_02_u"
        close(10)
        open(unit=10, file="submodule_object_02_m.f90", status="replace")
        write(10, '(a)') "module submodule_object_02_m"
        write(10, '(a)') "character(len=5) :: names(2) = [""alpha"", ""beta ""]"
        write(10, '(a)') "character(len=5) :: label = ""start"""
        write(10, '(a)') "integer :: total = 100"
        write(10, '(a)') "interface"
        write(10, '(a)') "module subroutine hello(k)"
        write(10, '(a)') "integer, intent(out) :: k"
        write(10, '(a)') "end subroutine hello"
        write(10, '(a)') "module subroutine bye(k)"
        write(10, '(a)') "integer, intent(out) :: k"
        write(10, '(a)') "end subroutine bye"
        write(10, '(a)') "end interface"
        write(10, '(a)') "contains"
        write(10, '(a)') "integer function helper()"
        write(10, '(a)') "helper = 3"
        write(10, '(a)') "end function helper"
        write(10, '(a)') "end module submodule_object_02_m"
        close(10)
        open(unit=10, file="submodule_object_02_s.f90", status="replace")
        write(10, '(a)') "submodule (submodule_object_02_m) submodule_object_02_s"
        write(10, '(a)') "use submodule_object_02_u, only: bump"
        write(10, '(a)') "integer :: counter = 7"
        write(10, '(a)') "contains"
        write(10, '(a)') "module subroutine hello(k)"
        write(10, '(a)') "integer, intent(out) :: k"
        write(10, '(a)') "counter = counter + 1"
        write(10, '(a)') "names(2) = ""gamma"""
        write(10, '(a)') "label = ""done """
        write(10, '(a)') "total = total + 1"
        write(10, '(a)') "k = bump(counter) + helper()"
        write(10, '(a)') "end subroutine hello"
        write(10, '(a)') "end submodule submodule_object_02_s"
        close(10)
        open(unit=10, file="submodule_object_02_t.f90", status="replace")
        write(10, '(a)') "submodule (submodule_object_02_m:submodule_object_02_s) submodule_object_02_t"
        write(10, '(a)') "contains"
        write(10, '(a)') "module subroutine bye(k)"
        write(10, '(a)') "integer, intent(out) :: k"
        write(10, '(a)') "k = counter"
        write(10, '(a)') "end subroutine bye"
        write(10, '(a)') "end submodule submodule_object_02_t"
        close(10)
        open(unit=10, file="submodule_object_02_p.f90", status="replace")
        write(10, '(a)') "program p"
        write(10, '(a)') "use submodule_object_02_m, only: hello, bye, names, label, total"
        write(10, '(a)') "integer :: k"
        write(10, '(a)') "call hello(k)"
        write(10, '(a)') "if (k /= 12) error stop 1"
        write(10, '(a)') "call hello(k)"
        write(10, '(a)') "if (k /= 13) error stop 2"
        write(10, '(a)') "call bye(k)"
        write(10, '(a)') "if (k /= 9) error stop 3"
        write(10, '(a)') "if (names(1) /= ""alpha"" .or. names(2) /= ""gamma"") error stop 4"
        write(10, '(a)') "if (label /= ""done "") error stop 5"
        write(10, '(a)') "if (total /= 102) error stop 6"
        write(10, '(a)') "end program p"
        close(10)
        open(unit=10, file="submodule_object_02_save_m.f90", status="replace")
        write(10, '(a)') "module submodule_object_02_save_m"
        write(10, '(a)') "interface"
        write(10, '(a)') "module function tick() result(n)"
        write(10, '(a)') "integer :: n"
        write(10, '(a)') "end function tick"
        write(10, '(a)') "end interface"
        write(10, '(a)') "end module submodule_object_02_save_m"
        close(10)
        open(unit=10, file="submodule_object_02_save_s.f90", status="replace")
        write(10, '(a)') "submodule (submodule_object_02_save_m) submodule_object_02_save_s"
        write(10, '(a)') "contains"
        write(10, '(a)') "module function tick() result(n)"
        write(10, '(a)') "integer :: n"
        write(10, '(a)') "integer, save :: count = 0"
        write(10, '(a)') "count = count + 1"
        write(10, '(a)') "n = count"
        write(10, '(a)') "end function tick"
        write(10, '(a)') "end submodule submodule_object_02_save_s"
        close(10)
        open(unit=10, file="submodule_object_02_save_x.f90", status="replace")
        write(10, '(a)') "subroutine submodule_object_02_save_x(n)"
        write(10, '(a)') "use submodule_object_02_save_m, only: tick"
        write(10, '(a)') "integer, intent(out) :: n"
        write(10, '(a)') "n = tick()"
        write(10, '(a)') "end subroutine submodule_object_02_save_x"
        close(10)
        open(unit=10, file="submodule_object_02_save_p.f90", status="replace")
        write(10, '(a)') "program save_p"
        write(10, '(a)') "use submodule_object_02_save_m, only: tick"
        write(10, '(a)') "interface"
        write(10, '(a)') "subroutine submodule_object_02_save_x(n)"
        write(10, '(a)') "integer, intent(out) :: n"
        write(10, '(a)') "end subroutine submodule_object_02_save_x"
        write(10, '(a)') "end interface"
        write(10, '(a)') "integer :: a, b, c"
        write(10, '(a)') "a = tick()"
        write(10, '(a)') "call submodule_object_02_save_x(b)"
        write(10, '(a)') "c = tick()"
        write(10, '(a)') "if (a /= 1 .or. b /= 2 .or. c /= 3) error stop 1"
        write(10, '(a)') "end program save_p"
        close(10)
    end subroutine write_sources
    subroutine compile(compiler, name)
        character(len=*), intent(in) :: compiler, name
        call run(compiler, "-c submodule_object_02_" // name // ".f90 -o submodule_object_02_" &
            // name // ".o", 10)
    end subroutine compile
    subroutine compile_with_flags(compiler, flags, name)
        character(len=*), intent(in) :: compiler, flags, name
        call run(compiler, trim(flags) // " -c submodule_object_02_" // name // &
            ".f90 -o submodule_object_02_" // name // ".o", 10)
    end subroutine compile_with_flags
    subroutine link_and_run(compiler, main_object, binary, code)
        character(len=*), intent(in) :: compiler, main_object, binary
        integer, intent(in) :: code
        call run(compiler, main_object // " submodule_object_02_u.o submodule_object_02_m.o " // &
            "submodule_object_02_s.o submodule_object_02_t.o -o " // binary, code)
        call run_binary(binary, code + 20)
    end subroutine link_and_run
    subroutine run(compiler, args, code)
        character(len=*), intent(in) :: compiler, args
        integer, intent(in) :: code
        character(len=4096) :: cmd
        integer :: exitstat
        cmd = '"' // trim(compiler) // '" ' // args
        call execute_command_line(trim(cmd), exitstat=exitstat)
        if (exitstat /= 0) then
            print *, "failed: ", trim(cmd)
            error stop code
        end if
    end subroutine run
    subroutine run_binary(binary, code)
        character(len=*), intent(in) :: binary
        integer, intent(in) :: code
        integer :: exitstat
        call execute_command_line("./" // binary, exitstat=exitstat)
        if (exitstat /= 0) then
            print *, "failed: ", binary
            error stop code
        end if
    end subroutine run_binary
end program submodule_object_02
