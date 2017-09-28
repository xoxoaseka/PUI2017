from __future__ import print_function
import sys
import os
import json
try:
    import urllib2 as urllib
except ImportError:
    import urllib.request as urllib

api_key = sys.argv[1]
bus_no = sys.argv[2]
bus_info = sys.argv[3]

bus_info = open(bus_info, 'w')
bus_info.write('Latitude, Longitude, Stop Name, Stop Status\n')

url = 'http://bustime.mta.info/api/siri/vehicle-monitoring.json?key=' + api_key + '&VehicleMonitoringDetailLevel=calls&LineRef=' + bus_no

response = urllib.urlopen(url)
data = response.read().decode('utf-8')
data = json.loads(data)

bus_data = data['Siri']['ServiceDelivery']['VehicleMonitoringDelivery'][0]['VehicleActivity']
active_bus_count = len(bus_data)

for i in range(active_bus_count):
    latitude = str(bus_data[i]['MonitoredVehicleJourney']['VehicleLocation']['Latitude'])
    longtitude = str(bus_data[i]['MonitoredVehicleJourney']['VehicleLocation']['Longitude'])
    if (bus_data[i]['MonitoredVehicleJourney']['OnwardCalls'] =={}):
        stop = 'N/A'
        status = 'N/A'
    else:
        stop = str(bus_data[i]['MonitoredVehicleJourney']['OnwardCalls']['OnwardCall'][0]['StopPointName'])
        status = str(bus_data[i]['MonitoredVehicleJourney']['OnwardCalls']['OnwardCall'][0]['Extensions']['Distances']['PresentableDistance'])
    bus_info.write(latitude + ',' + longtitude + ',' + stop + ',' + status + '\n')

print('Done!')
