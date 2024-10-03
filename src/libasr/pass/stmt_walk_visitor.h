#include <libasr/pass/pass_utils.h>

namespace LCompilers {

namespace ASR {
    template <class StructType>
    class StatementWalkVisitor : public PassUtils::PassVisitor<StructType>
    {

    public:

        StatementWalkVisitor(Allocator &al_) : PassUtils::PassVisitor<StructType>(al_, nullptr) {
        }

        void visit_WhileLoop( ASR::WhileLoop_t &x) {
            PassUtils::PassVisitor<StructType>::transform_stmts(x.m_body, x.n_body);
        }

        void visit_DoLoop( ASR::DoLoop_t &x) {
            PassUtils::PassVisitor<StructType>::transform_stmts(x.m_body, x.n_body);
        }
    };
} // namespace ASR

} // namespace LCompilers
