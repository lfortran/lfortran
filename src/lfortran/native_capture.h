#ifndef LFORTRAN_NATIVE_CAPTURE_H
#define LFORTRAN_NATIVE_CAPTURE_H
#include <functional>
#include <memory>
#include <string>

namespace LCompilers::LFortran {
class NativeCapture {
    struct Impl;
    std::unique_ptr<Impl> impl;
public:
    explicit NativeCapture(std::function<void(const std::string &,
        const std::string &)> sink);
    ~NativeCapture();
    void fence(bool final = false);
};
}
#endif
