// Compile with:
// clang++ -std=c++20 -o lsp_server lsp_server.cpp

#include <iostream>
#include <string>
#include "json.hpp"

using json = nlohmann::json;

// Read the header until "\r\n\r\n" to determine content length
std::string read_header() {
    std::string header;
    std::string line;
    // Read lines until we hit the blank line ("\r\n")
    while (std::getline(std::cin, line) && line != "\r") {
        header += line + "\n";
    }
    return header;
}

// Extract content length from the header
size_t get_content_length(const std::string& header) {
    size_t pos = header.find("Content-Length: ");
    if (pos != std::string::npos) {
        pos += 16; // Length of "Content-Length: "
        size_t end = header.find('\n', pos);
        std::string length_str = header.substr(pos, end - pos);
        return std::stoi(length_str);
    }
    throw std::runtime_error("Invalid header: Content-Length not found");
}

// Read the JSON message body based on content length
std::string read_message(size_t length) {
    std::string message(length, '\0');
    std::cin.read(&message[0], length);
    return message;
}

// Send a response with the correct LSP header
void send_response(const json& response) {
    std::string response_str = response.dump();
    std::string header = "Content-Length: " + std::to_string(response_str.size()) + "\r\n\r\n";
    std::cout << header << response_str << std::flush;
}

int main() {
    while (true) {
        // Read and parse the incoming message
        std::string header = read_header();
        size_t length = get_content_length(header);
        std::string message_str = read_message(length);
        json message = json::parse(message_str);

        std::string method = message["method"];
        int id = message["id"];

        // Process the request
        if (method == "initialize") {
            json result = {{"capabilities", json::object()}};
            send_response({{"jsonrpc", "2.0"}, {"id", id}, {"result", result}});
        } else if (method == "ping") {
            send_response({{"jsonrpc", "2.0"}, {"id", id}, {"result", "pong"}});
        } else if (method == "shutdown") {
            send_response({{"jsonrpc", "2.0"}, {"id", id}, {"result", nullptr}});
            break; // Exit after sending shutdown response
        }
    }
    return 0;
}
