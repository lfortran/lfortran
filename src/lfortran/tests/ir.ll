; ModuleID = 'LFortran'
source_filename = "LFortran"

@0 = private unnamed_addr constant [2 x i8] c" \00", align 1
@1 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@2 = private unnamed_addr constant [5 x i8] c"%s%s\00", align 1

define i32 @main(i32 %0, i8** %1) {
.entry:
  %a = alloca i32, align 4
  %b = alloca i32, align 4
  call void @_lpython_call_initial_functions(i32 %0, i8** %1)
  %a1 = alloca i32, align 4
  %b2 = alloca i32, align 4
  store i64 1, i32* %a1, align 4
  store i32 2, i32* %b2, align 4
  %2 = load i32, i32* %a1, align 4
  %3 = load i32, i32* %b2, align 4
  %4 = add i32 %2, %3
  %5 = sext i32 %4 to i64
  %6 = call i8* (i32, i8*, ...) @_lcompilers_string_format_fortran(i32 2, i8* null, i32 2, i64 %5)
  call void (i8*, ...) @_lfortran_printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @2, i32 0, i32 0), i8* %6, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @1, i32 0, i32 0))
  br label %return

return:                                           ; preds = %.entry
  ret i32 0
}

declare void @_lpython_call_initial_functions(i32, i8**)

declare i8* @_lcompilers_string_format_fortran(i32, i8*, ...)

declare void @_lfortran_printf(i8*, ...)
