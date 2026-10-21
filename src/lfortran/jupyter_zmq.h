#ifndef LFORTRAN_JUPYTER_ZMQ_H
#define LFORTRAN_JUPYTER_ZMQ_H

#include <lfortran/native_session.h>

namespace LCompilers::LFortran {
std::unique_ptr<SessionAdapter> jupyter_server(const std::string &file);
std::unique_ptr<SessionClient> connect_jupyter_session(const std::string &file);
// A bridge owns only transport state, never a FortranEvaluator.
class JupyterBridge : public SessionEndpoint, public SessionAdapter {};
std::unique_ptr<JupyterBridge> jupyter_bridge(const std::string &file);
bool is_local_connection_file(const std::string &file);
}
#endif
