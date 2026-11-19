#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/intrinsic_array_functions.h>

#include <map>
#include <string>
#include <tuple>

namespace LCompilers {

namespace ASRUtils {

#define ARRAY_INTRINSIC_NAME_CASE(X)                                            \
    case (static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::X)) : {       \
        return #X;                                                              \
    }

std::string get_array_intrinsic_name(int64_t x) {
    switch (x) {
        ARRAY_INTRINSIC_NAME_CASE(Any)
        ARRAY_INTRINSIC_NAME_CASE(All)
        ARRAY_INTRINSIC_NAME_CASE(Iany)
        ARRAY_INTRINSIC_NAME_CASE(Iall)
        ARRAY_INTRINSIC_NAME_CASE(Norm2)
        ARRAY_INTRINSIC_NAME_CASE(MatMul)
        ARRAY_INTRINSIC_NAME_CASE(MaxLoc)
        ARRAY_INTRINSIC_NAME_CASE(MaxVal)
        ARRAY_INTRINSIC_NAME_CASE(MinLoc)
        ARRAY_INTRINSIC_NAME_CASE(MinVal)
        ARRAY_INTRINSIC_NAME_CASE(FindLoc)
        ARRAY_INTRINSIC_NAME_CASE(Product)
        ARRAY_INTRINSIC_NAME_CASE(Shape)
        ARRAY_INTRINSIC_NAME_CASE(Coshape)
        ARRAY_INTRINSIC_NAME_CASE(Sum)
        ARRAY_INTRINSIC_NAME_CASE(Iparity)
        ARRAY_INTRINSIC_NAME_CASE(Transpose)
        ARRAY_INTRINSIC_NAME_CASE(Pack)
        ARRAY_INTRINSIC_NAME_CASE(Unpack)
        ARRAY_INTRINSIC_NAME_CASE(Count)
        ARRAY_INTRINSIC_NAME_CASE(Parity)
        ARRAY_INTRINSIC_NAME_CASE(DotProduct)
        ARRAY_INTRINSIC_NAME_CASE(Cshift)
        ARRAY_INTRINSIC_NAME_CASE(Eoshift)
        ARRAY_INTRINSIC_NAME_CASE(Spread)
        ARRAY_INTRINSIC_NAME_CASE(Reduce)
        default : {
            throw LCompilersException("pickle: intrinsic_id not implemented");
        }
    }
}

namespace IntrinsicArrayFunctionRegistry {

    static const std::map<int64_t, std::tuple<impl_function,
            verify_array_function>>& get_intrinsic_function_by_id_db() {
        static const std::map<int64_t, std::tuple<impl_function,
                verify_array_function>> intrinsic_function_by_id_db = {
        {static_cast<int64_t>(IntrinsicArrayFunctions::Any),
            {&Any::instantiate_Any, &Any::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::All),
            {&All::instantiate_All, &All::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Iany),
            {&Iany::instantiate_Iany, &Iany::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Iall),
            {&Iall::instantiate_Iall, &Iall::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Norm2),
            {&Norm2::instantiate_Norm2, &Norm2::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::MatMul),
            {&MatMul::instantiate_MatMul, &MatMul::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::MaxLoc),
            {&MaxLoc::instantiate_MaxLoc, &MaxLoc::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::MaxVal),
            {&MaxVal::instantiate_MaxVal, &MaxVal::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::MinLoc),
            {&MinLoc::instantiate_MinLoc, &MinLoc::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::FindLoc),
            {&FindLoc::instantiate_FindLoc, &FindLoc::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::MinVal),
            {&MinVal::instantiate_MinVal, &MinVal::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Product),
            {&Product::instantiate_Product, &Product::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Shape),
            {&Shape::instantiate_Shape, &Shape::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Coshape),
            {&Coshape::instantiate_Coshape, &Coshape::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Sum),
            {&Sum::instantiate_Sum, &Sum::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Iparity),
            {&Iparity::instantiate_Iparity, &Iparity::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Transpose),
            {&Transpose::instantiate_Transpose, &Transpose::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Pack),
            {&Pack::instantiate_Pack, &Pack::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Unpack),
            {&Unpack::instantiate_Unpack, &Unpack::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Count),
            {&Count::instantiate_Count, &Count::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Parity),
            {&Parity::instantiate_Parity, &Parity::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::DotProduct),
            {&DotProduct::instantiate_DotProduct, &DotProduct::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Cshift),
            {&Cshift::instantiate_Cshift, &Cshift::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Eoshift),
            {&Eoshift::instantiate_Eoshift, &Eoshift::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Spread),
            {&Spread::instantiate_Spread, &Spread::verify_args}},
        {static_cast<int64_t>(IntrinsicArrayFunctions::Reduce),
            {&Reduce::instantiate_Reduce, &Reduce::verify_args}},
        };
        return intrinsic_function_by_id_db;
    }

    static const std::map<std::string, std::tuple<create_intrinsic_function,
            eval_intrinsic_function>>& get_function_by_name_db() {
        static const std::map<std::string, std::tuple<create_intrinsic_function,
                eval_intrinsic_function>> function_by_name_db = {
        {"any", {&Any::create_Any, &Any::eval_Any}},
        {"all", {&All::create_All, &All::eval_All}},
        {"iany", {&Iany::create_Iany, &Iany::eval_Iany}},
        {"iall", {&Iall::create_Iall, &Iall::eval_Iall}},
        {"norm2", {&Norm2::create_Norm2, &Norm2::eval_Norm2}},
        {"matmul", {&MatMul::create_MatMul, &MatMul::eval_MatMul}},
        {"maxloc", {&MaxLoc::create_MaxLoc, nullptr}},
        {"maxval", {&MaxVal::create_MaxVal, &MaxVal::eval_MaxVal}},
        {"minloc", {&MinLoc::create_MinLoc, nullptr}},
        {"findloc", {&FindLoc::create_FindLoc, nullptr}},
        {"minval", {&MinVal::create_MinVal, &MinVal::eval_MinVal}},
        {"product", {&Product::create_Product, &Product::eval_Product}},
        {"shape", {&Shape::create_Shape, &Shape::eval_Shape}},
        {"coshape", {&Coshape::create_Coshape, &Coshape::eval_Coshape}},
        {"sum", {&Sum::create_Sum, &Sum::eval_Sum}},
        {"iparity", {&Iparity::create_Iparity, &Iparity::eval_Iparity}},
        {"cshift", {&Cshift::create_Cshift, &Cshift::eval_Cshift}},
        {"eoshift", {&Eoshift::create_Eoshift, &Eoshift::eval_Eoshift}},
        {"spread", {&Spread::create_Spread, &Spread::eval_Spread}},
        {"transpose", {&Transpose::create_Transpose, &Transpose::eval_Transpose}},
        {"pack", {&Pack::create_Pack, &Pack::eval_Pack}},
        {"unpack", {&Unpack::create_Unpack, &Unpack::eval_Unpack}},
        {"count", {&Count::create_Count, &Count::eval_Count}},
        {"parity", {&Parity::create_Parity, &Parity::eval_Parity}},
        {"dot_product", {&DotProduct::create_DotProduct, &DotProduct::eval_DotProduct}},
        {"reduce", {&Reduce::create_Reduce, &Reduce::eval_Reduce}},
        };
        return function_by_name_db;
    }

    bool is_intrinsic_function(const std::string& name) {
        return get_function_by_name_db().find(name) != get_function_by_name_db().end();
    }

    create_intrinsic_function get_create_function(const std::string& name) {
        return  std::get<0>(get_function_by_name_db().at(name));
    }

    impl_function get_instantiate_function(int64_t id) {
        if( get_intrinsic_function_by_id_db().find(id) == get_intrinsic_function_by_id_db().end() ) {
            return nullptr;
        }
        return std::get<0>(get_intrinsic_function_by_id_db().at(id));
    }

    verify_array_function get_verify_function(int64_t id) {
        return std::get<1>(get_intrinsic_function_by_id_db().at(id));
    }

    // The function that computes the compile-time value of the intrinsic
    // `id` from the values of its arguments (`m_args` of its
    // IntrinsicArrayFunction) and its type; nullptr for the intrinsics
    // without one.
    eval_intrinsic_function get_eval_function(int64_t id) {
        switch (static_cast<IntrinsicArrayFunctions>(id)) {
            case IntrinsicArrayFunctions::Any: return &Any::eval_Any;
            case IntrinsicArrayFunctions::All: return &All::eval_All;
            case IntrinsicArrayFunctions::Iany: return &Iany::eval_Iany;
            case IntrinsicArrayFunctions::Iall: return &Iall::eval_Iall;
            case IntrinsicArrayFunctions::Norm2: return &Norm2::eval_Norm2;
            case IntrinsicArrayFunctions::MatMul: return &MatMul::eval_MatMul;
            case IntrinsicArrayFunctions::MaxVal: return &MaxVal::eval_MaxVal;
            case IntrinsicArrayFunctions::MinVal: return &MinVal::eval_MinVal;
            case IntrinsicArrayFunctions::Product: return &Product::eval_Product;
            case IntrinsicArrayFunctions::Shape: return &Shape::eval_Shape;
            case IntrinsicArrayFunctions::Coshape: return &Coshape::eval_Coshape;
            case IntrinsicArrayFunctions::Sum: return &Sum::eval_Sum;
            case IntrinsicArrayFunctions::Iparity: return &Iparity::eval_Iparity;
            case IntrinsicArrayFunctions::Transpose: return &Transpose::eval_Transpose;
            case IntrinsicArrayFunctions::Pack: return &Pack::eval_Pack;
            case IntrinsicArrayFunctions::Unpack: return &Unpack::eval_Unpack;
            case IntrinsicArrayFunctions::Count: return &Count::eval_Count;
            case IntrinsicArrayFunctions::Parity: return &Parity::eval_Parity;
            case IntrinsicArrayFunctions::DotProduct: return &DotProduct::eval_DotProduct;
            case IntrinsicArrayFunctions::Cshift: return &Cshift::eval_Cshift;
            case IntrinsicArrayFunctions::Eoshift: return &Eoshift::eval_Eoshift;
            case IntrinsicArrayFunctions::Spread: return &Spread::eval_Spread;
            case IntrinsicArrayFunctions::Reduce: return &Reduce::eval_Reduce;
            case IntrinsicArrayFunctions::MaxLoc:
            case IntrinsicArrayFunctions::MinLoc:
            case IntrinsicArrayFunctions::FindLoc:
                return nullptr;
            default: {
                LCOMPILERS_ASSERT(false);
                return nullptr;
            }
        }
    }

} // namespace IntrinsicArrayFunctionRegistry

} // namespace ASRUtils

} // namespace LCompilers

// In a unity build the next source follows in the same translation unit.
#undef ARRAY_INTRINSIC_NAME_CASE
