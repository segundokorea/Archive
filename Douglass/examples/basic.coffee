# Example command-line usage:
#   $ coffee examples/basic.coffee BUSTRACKER_API_KEY
#   BUSTRACKER_API_KEY
#   [ 'East Bound', 'West Bound' ]
#   [ { vid: 1548,
#       tmstmp: Mon, 16 Jan 2012 02:36:00 GMT,
#       lat: 41.78647994995117,
#       lon: -87.68205401102702,
#       hdg: 89,
#       pid: 1334,
#       rt: '59',
#       des: 'Ashland',
#       pdist: 17046 } ]
#   Mon, 16 Jan 2012 02:36:03 GMT
#   ...
#
tracker = require('../lib/douglass')(process.argv[2])

log = (err, data) ->
  throw err if err
  console.log data

console.log tracker.apiKey
# tracker.getTime log
# tracker.getVehicles {rt: 55}, log
# tracker.getRoutes log
# tracker.getDirections {rt: 55}, log
# tracker.getStops {rt: 55, dir: 'East Bound'}, log
tracker.getPatterns {rt: 55}, log
# tracker.getPredictions {vid: 1557}, log
# tracker.getBulletins {stpid: 456}, log
