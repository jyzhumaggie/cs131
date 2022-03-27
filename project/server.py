import asyncio
import argparse
import time
import logging
import aiohttp
import json
import const 


class Server:
    def __init__(self, server_name):
        self.name = server_name
        self.ip = const.LOCALHOST
        self.port = const.SERVERS[server_name]
        
        self.client_timestamps = dict() 
        self.client_messages = dict()
        logging.info("Initializing server {}'s log file".format(server_name))


    async def handle_input(self, reader, writer):
        while not reader.at_eof():
            data = await reader.readline()
            message = data.decode()
            # ignore empty requests
            if message == "": 
                continue
            parsed_msg = message.split()
            logging.info("{} recieved: {}".format(self.name, message))

            # AT messages propagated from other Servers
            if len(parsed_msg) == 6 and parsed_msg[0] == "AT":
                logging.info("Propagated message received")
                return_msg = None
                client_name = parsed_msg[3]
                # if already exist in dict
                if client_name in self.client_timestamps:
                    logging.info("Data for {} already exists".format(client_name))
                    # if data is new
                    if float(parsed_msg[5]) > self.client_timestamps[client_name]: 
                        # update, propagate
                        logging.info("Updating data for client {}, propagating new data...".format(parsed_msg[1]))
                        self.client_messages[client_name] = message
                        self.client_timestamps[client_name] = float(parsed_msg[5])
                        await self.propagate(message)
                    else:
                        # end of propagation
                        logging.info("Received message already... Stop propagation.")
                        pass
                else: 
                    # new client
                    logging.info("New client {}, propagating new data...".format(parsed_msg[1]))
                    self.client_timestamps[client_name] = float(parsed_msg[5])
                    self.client_messages[client_name] = message
                    await self.propagate(message)
            
            # if it's IAMAT/WHATSAT command
            elif len(parsed_msg) == 4:
                client_time = parsed_msg[3]
                if parsed_msg[0] == "IAMAT":
                    if self.isIAMAT(parsed_msg):
                        # print("HEHEHHEH")
                        diff = time.time() - float(client_time)
                        str_diff = ["", "+"][diff > 0] + str(diff)
                        return_msg = "AT {} {} {} {} {}".format(self.name, str_diff, parsed_msg[1], parsed_msg[2], client_time)
                        self.client_timestamps[parsed_msg[1]] = float(client_time)
                        self.client_messages[parsed_msg[1]] = return_msg
                        await self.propagate(return_msg)

                elif parsed_msg[0] == "WHATSAT":
                    if self.isWHATSAT(parsed_msg):
                        location = self.client_messages[parsed_msg[1]].split()[4]
                        radius = parsed_msg[2]
                        bound = parsed_msg[3]
                        places = await self.get_coordinates(location, radius, bound)
                        return_msg = "{}\n{}\n\n".format(self.client_messages[parsed_msg[1]], str(places).rstrip('\n'))
            
            else: 
                return_msg = "? " + message

            if return_msg != None:
                logging.info("Sending message to client: {}".format(return_msg))
                writer.write(return_msg.encode())
                await writer.drain()

        logging.info("Closing client socket")
        writer.close()

    def isIAMAT(self, strings):
        # ISO 6709 notation
        coord = strings[2]
        coords = [c for c in coord.replace('+', '-').split('-') if c != ""]
        if len(coords) != 2 or not is_numeric(coords[0]) or not is_numeric(coords[1]):
            return False
        # float
        if not is_numeric(strings[3]):
            return False
        return True
    
    
    def isWHATSAT(self, strings):
        if not is_numeric(strings[2]) or not is_numeric(strings[3]):
            # print("3,4th arg not num")
            return False
        if int(strings[2]) < 0 or int(strings[2]) > 50:
            # print("radius too large")
            return False
        # check information bound at most 20 items
        if int(strings[3]) < 0 or int(strings[3]) > 20:
            # print("times check 0<x<20")
            return False
        # check if current client exists
        if strings[1] not in self.client_timestamps:
            # print("no data yet")
            return False
        # print("Fine")
        return True


    async def run_forever(self):
        logging.info('Running server {}...'.format(self.name))
        server = await asyncio.start_server(self.handle_input, self.ip, self.port)
        # Serve requests until Ctrl+C is pressed
        async with server:
            await server.serve_forever()
        logging.info('Server {} closing...'.format(self.name))
        # Close the server
        server.close()

    async def propagate(self, message):
        # send message to every server connected
        for connection in const.CONNECTIONS[self.name]:
            try:
                _, writer = await asyncio.open_connection(const.LOCALHOST, const.SERVERS[connection])
                
                writer.write(message.encode())
                logging.info("Server {} sending to Server {}, message content: {}".format(self.name, connection, message))

                await writer.drain()
                writer.close()
                await writer.wait_closed()
                logging.info("Closing connection with peer Server {}".format(connection))

            except:
                logging.info("Error connecting with peer Server {}".format(connection))


    def parse_coordinates(self, location):
        dividing_point = max(location.rfind("+"), location.rfind("-"))
        return "{},{}".format(location[:dividing_point], location[dividing_point:])


    async def get_coordinates(self, location, radius, num):
        async with aiohttp.ClientSession() as session:
            coordinates = self.parse_coordinates(location)
            if coordinates == None: 
                print("Wrong coordinate input")
                exit()
            logging.info("Finding places at location {}".format(coordinates))
            url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={}&radius={}&key={}'.format(coordinates, radius, const.API_KEY)
            async with session.get(url) as response:
                result_object = await response.json(loads=json.loads)
            logging.info("Retrieved {} place(s) with in the given radius".format(len(result_object["results"])))
            if len(result_object["results"]) > int(num):
                result_object["results"] = result_object["results"][:int(num)]
            return str(json.dumps(result_object, indent=4))

def is_numeric(str):
    try:
        float(str)
        return True
    except ValueError:
        return False


def main():
    parser = argparse.ArgumentParser('CS131 Project Argument Parser')
    parser.add_argument('server_name', type=str, help='required server name input')
    args = parser.parse_args()
    if not args.server_name in const.SERVERS:
        print("Invalid Server Input {}".format(args.server_name))
        exit()
    logging.basicConfig(filename="Server_{}.log".format(args.server_name), format='%(levelname)s: %(message)s', filemode='w+', level=logging.INFO)
    server = Server(args.server_name)
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    main()