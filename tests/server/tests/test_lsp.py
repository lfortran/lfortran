import subprocess
import json
import re
import pytest

class LSPClient:
    def __init__(self, server_path):
        # Launch the server with pipes for stdin, stdout, and stderr
        self.proc = subprocess.Popen(
            server_path,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )

    def send_message(self, message):
        # Serialize JSON and create header
        json_str = json.dumps(message)
        header = f"Content-Length: {len(json_str)}\r\n\r\n"
        # Log the outgoing header and message
        print(f"Client sending header: {header}", end='')
        print(f"Client sending message: {json_str}")
        # Write header and message, then flush
        self.proc.stdin.write(header.encode('utf-8'))
        self.proc.stdin.write(json_str.encode('utf-8'))
        self.proc.stdin.flush()

    def receive_message(self):
        # Read header until "\r\n\r\n"
        header = b''
        while not header.endswith(b'\r\n\r\n'):
            byte = self.proc.stdout.read(1)
            if byte == b'':
                raise EOFError("Server closed stdout unexpectedly")
            header += byte
        # Parse content length
        match = re.search(br'Content-Length: (\d+)\r\n\r\n', header)
        if not match:
            raise ValueError("Invalid header")
        length = int(match.group(1))
        # Read the message body
        message = self.proc.stdout.read(length)
        # Log the incoming header and message
        print(f"Client received header: {header.decode('utf-8')}", end='')
        print(f"Client received message: {message.decode('utf-8')}")
        return json.loads(message.decode('utf-8'))

    def close(self):
        # Close pipes and wait for the server to exit
        self.proc.stdin.close()
        self.proc.stdout.close()
        self.proc.wait()
        # Check for server errors
        stderr = self.proc.stderr.read()
        if stderr != "":
            print()
            print("Server stderr:")
            print(stderr.decode())
        #assert stderr == b'', f"Server produced error output: {stderr.decode()}"

def test_lsp():
    # Replace with the actual path to your compiled server executable
    client = LSPClient('examples/lsp_server')
    try:
        # Test 1: Send initialize request
        init_request = {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}}
        client.send_message(init_request)
        init_response = client.receive_message()
        assert init_response["id"] == 1
        assert "result" in init_response
        assert "capabilities" in init_response["result"]

        # Test 2: Send ping request
        ping_request = {"jsonrpc": "2.0", "id": 2, "method": "ping"}
        client.send_message(ping_request)
        ping_response = client.receive_message()
        assert ping_response["id"] == 2
        assert ping_response["result"] == "pong"

        # Test 3: Send shutdown request
        shutdown_request = {"jsonrpc": "2.0", "id": 3, "method": "shutdown"}
        client.send_message(shutdown_request)
        shutdown_response = client.receive_message()
        assert shutdown_response["id"] == 3
        assert shutdown_response["result"] is None
    finally:
        # Ensure the server shuts down cleanly
        client.close()
