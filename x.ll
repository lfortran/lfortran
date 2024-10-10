define i64 @f1()
{
    ; FAIL: "=x" is incorrect syntax
    %1 =x alloca i64
}
