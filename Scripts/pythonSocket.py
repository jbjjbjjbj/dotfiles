import socket
sock = socket.socket()

server_addr = ('192.168.43.86', 5001)
sock.bind(server_addr)

sock.listen(1)
print("Waiting for connection on " + str(server_addr))
while True:
	connection, client_address = sock.accept()
	try:
		print ('connection from', client_address)

		# Receive the data in small chunks and retransmit it
		while True:
			data = connection.recv(16)
			print('received ' ,data)
			if not data:
				print('no more data from', client_address)
				break
	finally:
		# Clean up the connection
		connection.close()