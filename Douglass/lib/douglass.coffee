# Douglass.
# douglass.coffee
# Node.js API for the CTA BusTracker
# Sean Clemmer
# 0.1.1
#
ent      = require 'ent'
http     = require 'http'
util     = require 'util'
xmlp     = require 'htmlparser2'
query    = require 'querystring'
score    = require 'underscore'
traverse = require 'traverse'


# Helper for generating the options object we send in the HTTP request
# apiKey: (string, required) BusTracker Developer API access key
# action: (string, required) API action for the BusTracker request, like 'gettime'
# params: (object) Additional parameters for the HTTP request
# Returns an object or undefined
createOptions = (apiKey, action, params={}) ->
  return if apiKey is undefined or action is undefined
  params.key = apiKey
  params     = query.stringify params
  params     = "?#{params}" unless params is ''
  return \
    host: 'www.ctabustracker.com'
    path: "/bustime/api/v1/#{action}#{params}"


# Create a JavaScript Date from a timestamp
# stamp: (string, required) Timestamp to convert, formatted YYYYMMDD HH:MM:SS
dateFromStamp = (stamp) ->
  return if stamp is undefined
  year    = stamp.slice 0, 4
  month   = stamp.slice 4, 6
  day     = stamp.slice 6, 8
  hour    = stamp.slice 9, 11
  minute  = stamp.slice 12, 14
  second  = stamp.slice 15, 17
  return new Date(year, month, day, hour, minute, second)


# Process the parsed XML (in JSON) into some more useful JSON
# callback: (function, required) Callback to proxy the processed data through
# err: (function, required) Incoming errors for callback
# rawData: (string, required) Incoming data for callback
processWithCallback = (callback, err, rawData) ->
  # Fail fast! Pass any errors right through to the client
  return if callback is undefined or rawData is undefined
  if err
    callback err
    return

  convertTextTags = (node) ->
    if node and node.type and node.type is 'text'
      this.update(node.data, true)

  convertChildren = (node) ->
    if node and node.type and node.children
      newNode  = {}
      children = node.children
      children = children[0] if children.length is 1
      newNode[node.name] = traverse(children).map(convertChildren)
      this.update(newNode, true)

  response = traverse(rawData[1]).map(convertTextTags)
  response = traverse(response).map(convertChildren)
  response = response['bustime-response']
  console.log score.pluck(score.pluck(response, 'ptr')[0], 'pt')
  # console.log score.uniq(score.flatten(score.map(response, (o) -> score.keys(o))))
  # console.log score.groupBy(response, 'ptr')



# Proxy an HTTP request to BusTracker, passing the result through a JSON preprocessor
# apiKey: (string, required) BusTracker Developer API access key
# action: (string, required) API action for the BusTracker request, like 'gettime'
# params: (object) Additional parameters for the HTTP request
# callback: (function, required) Callback to proxy the request through
performRequest = (apiKey, action, params, callback) ->
  # Require apiKey, aciton, and callback
  errs = []
  errs.push 'Missing apiKey' if apiKey is undefined
  errs.push 'Missing action' if action is undefined
  errs.push 'Missing params' if params is undefined
  if errs.length isnt 0
    callback errs if callback
    return

  # If only three arguments passed, last argument should actually be the callback
  if callback is undefined
    callback = params
    params   = undefined

  # We're going to preprocess the JSON before handing it back to the client
  processor = score.bind processWithCallback, undefined, callback
  handler   = new xmlp.DefaultHandler processor
  parser    = new xmlp.Parser handler
  parse     = (response) ->
    response.on 'data', (chunk) -> parser.write chunk
    response.on 'end', -> parser.done()

  # Send our request to the BusTracker service
  options = createOptions apiKey, action, params
  http.request(options, parse).end()


# Shorthand to help curry performRequest() with its first two arguments
# apiKey: (string, required) BusTracker Developer API access key
# action: (string, required) API action for the BusTracker request, like 'gettime'
performAction = (apiKey, action) ->
  return if apiKey is undefined or action is undefined
  score.bind performRequest, undefined, apiKey, action


# Proxy our client's request through to the BusTracker API. Douglass abstracts
# away the HTTP requests, XML parsing, and data processing, leaving you with
# pretty, native-looking JSON that better reflects the BusTracker response.
#
# e.g. Compare this XML and the resulting JSON
#   <=  <?xml version="1.0"?>
#       <bustime-response>  
#         <route>
#           <rt>8A</rt>
#           <rtnm>South Halsted</rtnm>
#         </route>    
#         <route>
#           <rt>10</rt>
#           <rtnm>Museum of S & I</rtnm>
#         </route>
#       </bustime-response>
#
#   =>  [ { rt: '8A', rtnm: 'South Halsted' }, { rt: '10', rtnm: 'Museum of S &amp; I' } ]
#
# Use it like this:
#   tracker = require('../lib/douglass')('CTA_BUSTRACKER_API_KEY')
#   console.log tracker.apiKey
#   tracker.getTime (err, data) -> console.log data
#   tracker.getVehicles {vid: '1731,1097'}, (err, data) -> console.log data
#
# Note: The params object should use a string to represent lists, like getVehicles here
#
module.exports = (apiKey) ->
  apiKey:         apiKey
  getTime:        performAction apiKey, 'gettime'
  getVehicles:    performAction apiKey, 'getvehicles'
  getRoutes:      performAction apiKey, 'getroutes'
  getDirections:  performAction apiKey, 'getdirections'
  getStops:       performAction apiKey, 'getstops'
  getPatterns:    performAction apiKey, 'getpatterns'
  getPredictions: performAction apiKey, 'getpredictions'
  getBulletins:   performAction apiKey, 'getservicebulletins'
