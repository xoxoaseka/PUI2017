from __future__ import print_function
import json
import os
import sys
try:
    import urllib2 as urllib
except ImportError:
    import urllib.request as urllib

api_key = sys.argv[1]
bus_no = sys.argv[2]

url = 'http://bustime.mta.info/api/siri/vehicle-monitoring.json?key=' + api_key + '&VehicleMonitoringDetailLevel=calls&LineRef=' + bus_no

response = urllib.urlopen(url)
data = response.read().decode('utf-8')
data = json.loads(data)

bus_data = data['Siri']['ServiceDelivery']['VehicleMonitoringDelivery'][0]['VehicleActivity']
active_bus_count = len(bus_data)

print ('Bus Line: ' + str(bus_no))
print ('Number of active buses: ' + str(active_bus_count))
for i in range(active_bus_count):
	latitude = str(bus_data[i]['MonitoredVehicleJourney']['VehicleLocation']['Latitude'])
	longitude = str(bus_data[i]['MonitoredVehicleJourney']['VehicleLocation']['Longitude'])
	print('Bus ' + str(i) + ' is at latitude ' + latitude + ' and longitude ' + longitude)
