LOCALHOST = '127.0.0.1'

API_KEY = 'INSERT KEY HERE'

SERVERS = {    
    'Juzang': 10632,
    'Bernard': 10633,
    'Jaquez': 10634,
    'Campbell': 10635, 
    'Clark': 10636
}


CONNECTIONS = {
    'Juzang': ['Clark', 'Bernard', 'Campbell'],
    'Bernard': ['Juzang', 'Jaquez', 'Campbell'],
    'Jaquez': ['Clark', 'Bernard'],
    'Campbell': ['Juzang', 'Bernard'],
    'Clark': ['Juzang', 'Jaquez']
}

