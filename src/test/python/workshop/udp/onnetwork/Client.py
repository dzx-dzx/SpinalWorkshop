import socket
from threading import Thread

import time

from _socket import SOL_SOCKET, SO_REUSEADDR, SO_BROADCAST

RX_IP = open('ip.txt', 'r').read()
TX_IP = "255.255.255.255"
SERVER_PORT = 37984

sock = socket.socket(socket.AF_INET, # Internet
                     socket.SOCK_DGRAM) # UDP
sock.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
sock.setsockopt(SOL_SOCKET, SO_BROADCAST, 1)
sock.bind((RX_IP, 0))

def rxThread(sock,dummy):
    while True:
        data, addr = sock.recvfrom(2048)
        print ("received message:", data, addr)

try:
    Thread(target=rxThread, args=(sock,1)).start()
except Exception as errtxt:
    print (errtxt)

print(f"Send request to {TX_IP}:{SERVER_PORT}")
sock.sendto(b'\x11\x02', (TX_IP, SERVER_PORT))
print("Wait two seconds for answers")

time.sleep(3)
print("Done")