import sys
import datetime
import asyncio
import aiohttp
import json


APIKEY = 'AIzaSyAI4IRkMeECJEBQCKH6wrRiWu1X4k76xII'

PORT_NUMS = {
    'Riley': 15400,
    'Jaquez': 15401,
    'Bernard': 15402,
    'Campbell': 15403,
    'Juzang': 15404
}

CONNECTIONS = {
    'Riley': ['Jaquez', 'Juzang'],
    'Jaquez': ['Riley', 'Bernard'],
    'Bernard': ['Jaquez', 'Juzang', 'Campbell'],
    'Campbell': ['Bernard', 'Juzang'],
    'Juzang': ['Riley', 'Bernard', 'Campbell'],
}


async def fetch(url):
    async with aiohttp.ClientSession(
        connector=aiohttp.TCPConnector(
            ssl=False,
        ),
    ) as session:
        async with session.get(url) as response:
            response = await response.json()
            # print(response)
            return response


async def retrieve_nearby(coord, radius, place_num):
    coord_li = coord_trans(coord)
    coord_in = coord_li[0]+','+coord_li[1]
    url = f'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={coord_in}&radius={radius}&key={APIKEY}'
    write_log(f'Getting info at: {url}')
    places = await fetch(url)
    write_log(f'Retrieved {len(places["results"])} places')
    if len(places['results']) > int(place_num):
        places['results'] = places['results'][:int(place_num)]
    return str(json.dumps(places, indent=4))


def coord_trans(coord):
    ind = 0
    count = 0
    lat = ''
    lon = ''
    for c in coord:
        if c == '+' or c == '-':
            count = count + 1
            if count == 2:
                lat = coord[:(ind - 1)]
                lon = coord[ind:]
        ind = ind + 1
    # return f'{lat},{lon}'
    return [lat, lon]   # type str


def is_num(string):
    try:
        float(string)
        return True
    except ValueError:
        return False


def write_log(message):
    print(message)
    logFile.write(message)


def write_error(message):
    message = 'ERROR! ' + message
    print(message)
    logFile.write(message)


class Server:
    def __init__(self, server_name):
        self.server_name = server_name
        self.port = PORT_NUMS[server_name]
        self.clients = {}
        write_log(f'Opened server {server_name}')

    async def handle_cmd(self, reader, writer):
        while not reader.at_eof():
            data = await reader.readline()
            message = data.decode()
            addr = writer.get_extra_info('peername')
            write_log(f'Received from server {addr}: {message}')

            response = await self.parse_cmd(message)
            if response:
                writer.write(response.encode())
                write_log(f"Responded to client {addr}: {response}")
            writer.close()
            write_log(f'Closed connection with {addr}')

    async def parse_cmd(self, message):
        cmd_funcs = {
            'IAMAT': self.i_am_at,
            'WHATSAT': self.whats_at,
            'AT': self.at
        }
        arg_nums = {
            'IAMAT': 4,
            'WHATSAT': 4,
            'AT': 6
        }

        # check empty cmd, wrong cmd, and wrong number of args
        message_list = [msg for msg in message.strip().split() if msg]
        if len(message_list) == 0 or message_list[0] not in cmd_funcs:
            write_error(f'Invalid cmd: {message}')
            return f'? {message}'
        elif len(message_list) != arg_nums[message_list[0]]:
            write_error(f'Invalid arg num for {message_list[0]}: {len(message_list)}')
            return f'? {message}'

        func = cmd_funcs.get(message_list[0])
        try:
            response = await func(*message_list[1:])
            return response
        except Exception as e:
            write_error(f'failed at command: {e}')
            return f'? {message}'

    async def i_am_at(self, client, coord, timestamp):
        coords = coord_trans(coord)
        if len(coords) != 2 or not is_num(coords[0]) or not is_num(coords[1]):
            raise Exception(f'Invalid IAMAT coord: {coord}')
        if not is_num(timestamp):
            raise Exception(f'Invalid IAMAT timestamp: {timestamp}')

        time_delta = datetime.datetime.now().timestamp() - float(timestamp)
        if time_delta > 0:
            time_delta = '+' + str(time_delta)
        else:
            time_delta = str(time_delta)
        response = f'AT {self.server_name} {time_delta} {client} {coord} {timestamp}'

        self.clients[client] = {'timestamp': timestamp, 'message': response}
        await self.propagate(response)
        return response

    async def whats_at(self, client, radius, place_num):
        if not client in self.clients:
            raise Exception(f'Invalid WHATSAT client: {client}')
        if not is_num(radius) or float(radius) < 0 or float(radius) > 50:
            raise Exception(f'Invalid WHATSAT radius: {radius}')
        if not is_num(place_num) or float(place_num) < 0 or float(place_num) > 20:
            raise Exception(f'Invalid WHATSAT places num: {place_num}')

        at_cmd = self.clients[client]['message']
        coord = at_cmd.split()[4]
        places_str = (await retrieve_nearby(coord, radius, place_num)).rstrip('\n')
        return f'{at_cmd}\n{places_str}\n\n'

    async def at(self, server, time_delta, client, coord, timestamp):
        if client not in self.clients or timestamp > self.clients[client]['timestamp']:
            at_cmd = f'AT {server} {time_delta} {client} {coord} {timestamp}'
            self.clients[client] = {'timestamp': timestamp, 'message': at_cmd}
            write_log(f'Received propagation for {client}')
            await self.propagate(at_cmd)
        else:
            write_log(f'Redundant propagation for client {client}. Skipped.')


    async def propagate(self, message):
        for reachable_server in CONNECTIONS[self.server_name]:
            try:
                reader, writer = await asyncio.open_connection('127.0.0.1', PORT_NUMS[reachable_server])
                writer.write(message.encode())
                write_log(f'Propagating to {reachable_server}: {message}')

                writer.close()
                await writer.wait_closed()
                write_log(f'Closing connection with {reachable_server}')
            except:
                write_log(f'Failed to connect to server {reachable_server}')

    async def server_entry(self):
        server = await asyncio.start_server(self.handle_cmd, '127.0.0.1', self.port)
        addr = server.sockets[0].getsockname()
        write_log(f'{self.server_name} serving on {addr}')
        async with server:
            await server.serve_forever()

    def run(self):
        try:
            asyncio.run(self.server_entry())
        except KeyboardInterrupt:
            write_log(f'Server {self.server_name} closed.')


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("error: must specify name of server!")
        exit()

    global server_name
    server_name = sys.argv[1]
    print(server_name)
    if server_name not in PORT_NUMS:
        print(f"error: invalid server name {server_name}")
        exit()

    log = server_name + "-log.txt"
    global logFile
    open(log, "w").close()  # clears the file
    logFile = open(log, 'w')

    s = Server(server_name)
    s.run()
    logFile.close()

