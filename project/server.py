import asyncio
import aiohttp
import time 
import sys
import json
import string

# DELETE BEFORE COMMIT
API_KEY = "secret"
GOOGLE_URL = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key='
# ASSIGNED PORTS
HILL_PORT = 12215 
JAQUEZ_PORT = 12216 
SMITH_PORT = 12217 
CAMPBELL_PORT = 12218 
SINGLETON_PORT = 12219

clients = {}
# START A SERVER

async def main():
	global LOGFILE
	if (len(sys.argv) != 2):
		raise Exception("Must pass a single server name (Hill, Jaquez, Smith, Campbell or Singleton)")
	elif (sys.argv[1] == "Hill"):
		server = await asyncio.start_server(handle_server, host='127.0.0.1', port = HILL_PORT)
		LOGFILE = open("hill_log.out", 'a')
		LOGFILE.truncate(0)
		LOGFILE.write("STARTING HILL\n")
	elif sys.argv[1] == "Jaquez":
		server = await asyncio.start_server(handle_server, host='127.0.0.1', port = JAQUEZ_PORT)
		LOGFILE = open("jaquez_log.out", 'a')
		LOGFILE.truncate(0)
		LOGFILE.write("STARTING JAQUEZ\n")
	elif sys.argv[1] == "Smith":
		server = await asyncio.start_server(handle_server, host='127.0.0.1', port = SMITH_PORT)
		LOGFILE = open("smith_log.out", 'a')
		LOGFILE.truncate(0)
		LOGFILE.write("STARTING SMITH\n")
	elif sys.argv[1] == "Campbell":
		server = await asyncio.start_server(handle_server, host='127.0.0.1', port = CAMPBELL_PORT)
		LOGFILE = open("campbell_log.out", 'a')
		LOGFILE.truncate(0)
		LOGFILE.write("STARTING CAMPBELL\n")
	elif sys.argv[1] == "Singleton":
		server = await asyncio.start_server(handle_server, host='127.0.0.1', port = SINGLETON_PORT)
		LOGFILE = open("singleton_log.out", 'a')
		LOGFILE.truncate(0)
		LOGFILE.write("STARTING SINGLETON\n")
	else:
		raise Exception("Valid names are Hill, Jaquez, Smith, Campbell or Singleton, you said {}".format(sys.argv[1]))
	LOGFILE.flush()
	await server.serve_forever()
	LOGFILE.close()

async def flood (server_name, command) :
	command[0] = "FLOOD"
	flood_message = ' '.join(command)
	LOGFILE.write("Writing " + flood_message + " to all neighbors\n")
	LOGFILE.flush()
	# who is talking to who
	if (server_name == "Hill"):
		# jaquez and smith
		try:
			reader_jaquez, writer_jaquez = await asyncio.open_connection('127.0.0.1', JAQUEZ_PORT)
			writer_jaquez.write(flood_message.encode())
			writer_jaquez.close()
		except(ConnectionRefusedError):
			pass
		try: 
			reader_smith, writer_smith = await asyncio.open_connection('127.0.0.1', SMITH_PORT)
			writer_smith.write(flood_message.encode())
			writer_smith.close()
		except(ConnectionRefusedError):
			pass
	elif (server_name == "Jaquez"):
		# singleton hill
		try:
			reader_singleton, writer_singleton = await asyncio.open_connection('127.0.0.1', SINGLETON_PORT)
			writer_singleton.write(flood_message.encode())
			writer_singleton.close()
		except(ConnectionRefusedError):
			pass
		try:
			reader_hill, writer_hill = await asyncio.open_connection('127.0.0.1', HILL_PORT)
			writer_hill.write(flood_message.encode())
			writer_hill.close()
		except(ConnectionRefusedError):
			pass
	elif (server_name == "Smith"):
		# singleton campbell
		try:
			reader_singleton, writer_singleton = await asyncio.open_connection('127.0.0.1', SINGLETON_PORT)
			writer_singleton.write(flood_message.encode())
			writer_singleton.close()
		except(ConnectionRefusedError):
			pass
		try:
			reader_campbell, writer_campbell = await asyncio.open_connection('127.0.0.1', CAMPBELL_PORT)
			writer_campbell.write(flood_message.encode())
			writer_campbell.close()
		except(ConnectionRefusedError):
			pass
		try:
			reader_hill, writer_hill = await asyncio.open_connection('127.0.0.1', HILL_PORT)
			writer_hill.write(flood_message.encode())
			writer_hill.close()
		except(ConnectionRefusedError):
			pass
	elif (server_name == "Campbell"):
		# singleton smith
		try:
			reader_singleton, writer_singleton = await asyncio.open_connection('127.0.0.1', SINGLETON_PORT)
			writer_singleton.write(flood_message.encode())
			writer_singleton.close()
		except(ConnectionRefusedError):
			pass
		try:
			reader_smith, writer_smith = await asyncio.open_connection('127.0.0.1', SMITH_PORT)
			writer_smith.write(flood_message.encode())
			writer_smith.close()
		except(ConnectionRefusedError):
			pass
	elif (server_name == "Singleton"):
		# jaquez smith campbell
		try:
			reader_jaquez, writer_jaquez = await asyncio.open_connection('127.0.0.1', JAQUEZ_PORT)
			writer_jaquez.write(flood_message.encode())
			writer_jaquez.close()
		except(ConnectionRefusedError):
			pass
		try:
			reader_smith, writer_smith = await asyncio.open_connection('127.0.0.1', SMITH_PORT)
			writer_smith.write(flood_message.encode())
			writer_smith.close()
		except(ConnectionRefusedError):
			pass
		try:
			reader_campbell, writer_campbell = await asyncio.open_connection('127.0.0.1', CAMPBELL_PORT)
			writer_campbell.write(flood_message.encode())
			writer_campbell.close()
		except(ConnectionRefusedError):
			pass

async def handle_server(reader, writer):
	data = await reader.readline()
	command = data.decode()
	LOGFILE.write("Received command: " + command + '\n')
	LOGFILE.flush()
	command = command.split()
	server_name = sys.argv[1]
	message = ""
	if (command[0] == "WHATSAT"):
		if (len(command) != 4):
			message = "? " + data.decode()
		elif (not command[1] in clients):
			message = "? " + data.decode()
		else:
			saved_command = clients[command[1]]
			if ( float(saved_command[5]) > 0):
				saved_command = saved_command[4] + ' +' + saved_command[5] + ' ' + ' '.join(saved_command[1:4])
			else:
				saved_command = saved_command[4] + ' ' + saved_command[5] + ' ' + ' '.join(saved_command[1:4])
			# NEED TO FILL OUT JSON
			location = clients[command[1]][2]
			invalid = False
			if(location.find('-') == -1):
				index = location.rfind('+')
				location = location[:index] + ',' + location[index:]
			elif(location.find('+') == -1):
				index = location.rfind('-')
				location = location[:index] + ',' + location[index:]
			elif(location.find('-') != 0):
				index = location.rfind('-')
				location = location[:index] + ',' + location[index:]
			elif(location.find('+') != 0):
				index = location.rfind('+')
				location = location[:index] + ',' + location[index:]
			else:
				message = "? " + data.decode()
				invalid = True

			radius = int(command[3])
			url = GOOGLE_URL + '{0}&location={1}&radius={2}'.format(API_KEY, location, command[2])
			async with aiohttp.ClientSession(
				connector=aiohttp.TCPConnector(ssl=False,),) as session:
					async with session.get(url) as resp:
						json_response = await resp.json()
			json_response['results'] = json_response['results'][:radius]
			if(not invalid):
				message = "AT " + saved_command + '\n' + json.dumps(json_response, indent=4)
	if (command[0] == "IAMAT"):
		if (len(command) != 4): 
			message = "? " + data.decode()
		else:
			time_diff = time.time() - float(command[3])
			if(time_diff > 0):
				message = "AT " + server_name + " +" + str(time_diff) + " " + ' '.join(command[1:])
			else:
				message = "AT " + server_name + " " + str(time_diff) + " " + ' '.join(command[1:])
			# NEED TO FLUSH TO OTHER SERVERS
			command.append(server_name)
			command.append(str(time_diff))

			await flood(server_name, command)
			clients[command[1]] = command

	if (command[0] == "FLOOD"):
		if (not command[1] in clients):
			clients[command[1]] = command
			await flood(server_name, command)
		elif (clients[command[1]] != command):
			clients[command[1]] = command
			print(command)
			print(clients[command[1]])
			await flood(server_name, command)

	writer.write(message.encode())
	await writer.drain()
	writer.close()






if __name__ == '__main__':
	try:
		asyncio.run(main())
	except KeyboardInterrupt:
		pass
