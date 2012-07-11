// = Name
// tracker.js
// Douglass - Cross-browser, cross-platform alternative to CTA BusTracker
//
// = Description
// The ChicagoTransit module queries the CTA BusTracker Service using the
// official version 1.0 API. CTA uses the BusTime system to provide developers
// with real-time route information through BusTracker. The BusTracker Service
// allows deveopers to make HTTP requests and recieve XML responses. The CTA
// provides {official API documentation (PDF)}[http://www.transitchicago.com/assets/1/developer_center/BusTime_Developer_API_Guide.pdf],
// and you can request an access key at the {API homepage}[http://www.transitchicago.com/developers/bustracker.aspx].
//
// Requires:: jQuery (1.4.2)
// Version::  01
// Updated::  2010-03-23
// Author::   Sean Clemmer


// == Tracker
// JavaScript implementation of the CTA BusTracker system.
// Most methods use a callback function, which is loaded with the results of the query.
// Response errors from the system load the callback with an error property.
// == Example Usage
// tracker = new Tracker();
function Tracker() {

  // === Usage
  // Create a Tracker error and pass it to the callback
  // === Parameters
  // - message: Human-readable error description
  // - callback: Function to pass error with
  var create_error_with = function( message, callback ) {
    var error = {
      'message' : message,
      'error'   : true
    };
    callback.call( this, error );
  };


  // === Usage
  // Handle CTA errors and pass them to the callback
  // === Parameters
  // - response: XML document processed in the $.ajax request
  // - callback: Function to pass error with
  var handle_error_with = function( response, callback ) {
    if ( $( response ).find( 'error' ).length != 0 ) {
      create_error_with( 'CTA Error: ' + $( response ).find( 'error > msg' ).text(), callback );
      return true;
    } else {
      return false;
    }
  };


  // === Usage
  // Convert an XML response into a JSON object
  // === Parameters
  // - response: XML document processed in the $.ajax request
  // - root: Name of an XML tag to look up properties
  // - properties: JSON dictionary of CTA tags and property equivalents
  var jsonify = function( response, root, properties ) {
    var items = new Array();
    $( response ).find( root ).each( function() {
      var item = {};
      var this_response = $( this );
      $.each( properties, function( cta_tag, property ) {
        if ( property == 'delayed' ) {
          // I wonder if there is a better way of handling this case...
          item[property] = this_response.find( cta_tag ).text() == '' ? false : true;
        } else {
          item[property] = this_response.find( cta_tag ).text();
        }
      });
      items.push( item );
    });
    return items;
  };


  // === Usage
  // Construct a new instance of the Tracker.
  // === Parameters
  // - api_key: 25-digit CTA BusTracker API access key (required)
  // === Example Usage
  // tracker.initialize( CTA_API_KEY );
  this.initialize = function( api_key ) {
    this.api_key   = api_key;
    this.api_url   = 'http://www.ctabustracker.com/bustime/api/v1/';
    this.key_query = '?key=' + this.api_key;
  };


  // === Usage
  // Retrieve the system date and (local) time.
  // === Base URL
  // http://www.ctabustracker.com/bustime/api/v1/gettime
  // === Example Usage
  // tracker.time( function( time ) {
  //   alert( time );
  // });
  this.time = function( callback ) {
    var request = this.api_url + 'gettime' + this.key_query;

    $.ajax({
      url     : request,
      success : function( response ) {
        if ( !handle_error_with( response, callback ) ) {
          var time = $( response ).find( 'tm' ).text();
          callback.call( this, time);
        }
      }
    });
  };


  // === Usage
  // Retrieve information for vehicles serviced by the specified list or routes.
  // === Base URL
  // http://www.ctabustracker.com/bustime/api/v1/getvehicles
  // === Parameters
  // - routes: List of CTA route designators (not available with vehicles parameter)
  // - vehicles: List of CTA vehicle IDs (not available with routes parameter)
  // At least one required. If both routes and vehicles are provided, routes are used.
  // === Example Usage
  // tracker.vehicles( { 'routes': '55' },
  //   function( vehicles ) {
  //     if ( vehicles.error == true ) {
  //       alert( vehicles.message );
  //     } else {
  //       $.each( vehicles, function( index, vehicle ) {
  //         alert( vehicle.number );
  //       });
  //     }
  //   }
  // );
  this.vehicles = function( params, callback ) {
    var request = this.api_url + 'getvehicles' + this.key_query;
    
    if ( !params.routes && !params.vehicles ) {
      create_error_with( "No 'routes' or 'vehicles' provided", callback );
      return;
    }

    params.routes ? request += '&rt=' + params.routes : undefined;
    params.vehicles ? request += '&vidd=' + params.vehicles : undefined;

    $.ajax({
      url     : request,
      success : function( response ) {
        if ( !handle_error_with( response, callback ) ) {
          var vehicles = jsonify( response, 'vehicle', {
            'vid'    : 'number',
            'tmstmp' : 'timestamp',
            'lat'    : 'latitude',
            'lon'    : 'longitude',
            'hdg'    : 'heading',
            'pid'    : 'pattern',
            'pdist'  : 'distance',
            'rt'     : 'route',
            'des'    : 'destination',
            'dly'    : 'delayed'
          });
          callback.call( this, vehicles );
        }
      }
    });
  };


  // === Usage
  // Retrieve a list of routes serviced by the system.
  // === Base Url
  // http://www.ctabustracker.com/bustime/api/v1/getroutes
  // === Example Usage
  // tracker.routes(
  //   function( routes ) {
  //     if ( routes.error == true ) {
  //       alert( routes.message );
  //     } else {
  //       $.each( routes, function( index, route ) {
  //         alert( route.number );
  //       });
  //     }
  //   }
  // );
  this.routes = function( callback ) {
    var request = this.api_url + 'getroutes' + this.key_query;

    $.ajax({
      url     : request,
      success : function( response ) {
        if ( !handle_error_with( response, callback ) ) {
          var routes = jsonify( response, 'route', {
            'rt'   : 'number',
            'rtnm' : 'description'
          });
          callback.call( this, routes );
        }
      }
    });
  };


  // === Usage
  // Retrieve a set of directions serviced by the specified route.
  // === Base Url
  // http://www.ctabustracker.com/bustime/api/v1/getdirections
  // === Parameters
  // - route: Single route designator (required)
  // === Example usage
  // tracker.directions( { 'route': '55' },
  //  function( directions ) {
  //     if ( directions.error == true ) {
  //       alert( directions.message );
  //     } else {
  //       $.each( directions, function( index, direction ) {
  //         alert( direction.value );
  //       });
  //     }
  //   }
  // );
  this.directions = function( params, callback ) {
    var request = this.api_url + 'getdirections' + this.key_query;

    if ( params.route ) {
      request += '&rt=' + params.route;
    } else {
      create_error_with( "No 'route' provided", callback );
      return;
    }

    $.ajax({
      url     : request,
      success : function( response ) {
        if ( !handle_error_with( response, callback ) ) {
          // This would be nice to abstact, like the others...
          var directions = new Array();
          $( response ).find( 'dir' ).each( function() {
            var direction = {
              'value' : $( this ).text()
            };
            directions.push( direction );
          });
          callback.call( this, directions );
        }
      }
    });
  };


  // === Usage
  // Retrieve a set of stoped serviced by the specified route and direction.
  // === Base Url
  // http://www.ctabustracker.com/bustime/api/v1/getstops
  // === Parameters
  // - route: Single route designator (required)
  // - direction: Sing route direction (required)
  // == Example Usage
  // tracker.stops( { 'route': '55', 'direction': 'East bound' },
  //   function( stops ) {
  //     if ( stops.error == true ) {
  //       alert( stops.message );
  //     } else {
  //       $.each( stops, function( index, stop ) {
  //         alert( stop.number );
  //       });
  //     }
  //   }
  // );
  this.stops = function( params, callback ) {
    var request = this.api_url + 'getstops' + this.key_query;
    
    if ( params.route ) {
      if ( params.direction ) {
        request += '&rt=' + params.route + '&dir=' + params.direction;
      } else {
        create_error_with( "No 'direciton' provided", callback );
        return;
      }
    } else {
      create_error_with( "No 'route' provided", callback );
      return;
    }

    $.ajax({
      url     : request,
      success : function( response ) {
        if ( !handle_error_with( response, callback ) ) {
          var stops = jsonify( response, 'stop', {
              'stpid' : 'number',
              'stpnm' : 'description',
              'lat'   : 'latitidue',
              'lon'   : 'longitude'
          });
          callback.call( this, stops );
        }
      }
    });
  };


  // === Usage
  // Retrieve a set of patterns serviced by the specified list or routes.
  // Patterns are composed of geo-positional points that may be used to construct a map of the pattern (i.e. route variation).
  // === Base Url
  // http://www.ctabustracker.com/bustime/api/v1/getpatterns
  // === Parameters
  // - route: Single route designator (not available with patterns parameter)
  // - patterns: List of pattern IDs (not available with route parameter)
  // At least one required. If both route and patterns are provided, route is used.
  // === Example Usage
  // tracker.patterns( { 'route': '55' },
  //   function( patterns ) {
  //     if ( patterns.error == true ) {
  //       alert( patterns.message );
  //     } else {
  //       $.each( patterns, function( index, pattern ) {
  //         alert( pattern.number );
  //       });
  //     }
  //   }
  // );
  this.patterns = function( params, callback ) {
    var request = this.api_url + 'getpatterns' + this.key_query;

    if ( params.route || params.patterns ) {
      params.route   ? request += '&rt='  + params.route   : undefined;
      params.pattern ? request += '&pid=' + params.pattern : undefined;
    } else {
      create_error_with( "No 'route' or 'patterns' provided", callback );
      return;
    }

    $.ajax({
      url     : request,
      success : function( response ) {
        if ( !handle_error_with( response, callback ) ) {
          var patterns = jsonify( response, 'ptr', {
            'pid'   : 'number',
            'ln'    : 'length',
            'rtdir' : 'direction',
            'pt'    : 'points'
          });
          callback.call( this, patterns );
        }
      }
    });
  };

  // === Usage
  // Retrieve a set of predictions for the specified stops or vehicles.
  // === Base Url
  // http://www.ctabustracker.com/bustime/api/v1/getpredictions
  // === Parameters
  // - stops: List of stop IDs (not available with vehicles parameter)
  // - routes: List of route designators (optional, only available with stops parameter)
  // - vehicles: List of vehicle IDs (not available with stops parameter)
  // - ceiling: Maximum number of predictions to retrieve (optional)
  // At least stops or vehicles required. If both stops and vehiles are provided, stops are used.
  // === Example Usage
  // tracker.predictions( { 'vehicles': '1105' },
  //   function( predictions ) {
  //     if ( predictions.error == true ) {
  //       alert( predictions.message );
  //     } else {
  //       $.each( predictions, function( index, prediction ) {
  //         alert( prediction.route );
  //       });
  //     }
  //   }
  // );
  this.predictions = function( params, callback ) {
    var request = this.api_url + 'getpredictions' + this.key_query;

    if ( !params.stops && !params.vehicles ) {
      create_error_with( "No 'stops' or 'vehicles' provided", callback );
      return;
    } else {
      params.stops    ? request += '&stpid=' + params.stops    : undefined;
      params.routes   ? request += '&rt='    + params.routes   : undefined;
      params.vehicles ? request += '&vid='   + params.vehicles : undefined;
      params.ceiling  ? request += '&top='   + params.ceiling  : undefined;
    }

    $.ajax({
      url     : request,
      success : function( response ) {
        if ( !handle_error_with( response, callback ) ) {
          var predictions = jsonify( response, 'prd', {
            'tmstmp' : 'timestamp',
            'typ'    : 'type',
            'stpid'  : 'stop',
            'stpnm'  : 'description',
            'vid'    : 'vehicle',
            'dstp'   : 'distance',
            'rt'     : 'route',
            'des'    : 'destination',
            'prdtm'  : 'prediction',
            'dly'    : 'delayed'
          });
          callback.call( this, predictions );
        }
      }
    });
  };


  // === Usage
  // Retrieve a set of service bulletins in effect for the specified routes or stops.
  // === Base Url
  // http://www.ctabustracker.com/bustime/api/v1/getservicebulletins
  // === Parameters
  // - routes: List of route designators (required without stops parameter)
  // - direction: Single route direction (optional)
  // - stops: List of stop IDs (required without routes parameter)
  // === Example Usage
  // tracker.bulletins( { 'routes': '55' },
  //   function( bulletins ) {
  //     if ( bulletins.error == true ) {
  //       alert( bulletins.message );
  //     } else {
  //       $.each( bulletins, function( index, bulletin ) {
  //         alert( bulletin.number );
  //       });
  //     }
  //   }
  // );
  this.bulletins = function( params, callback ) {
    var request = this.api_url + 'getservicebulletins' + this.key_query;

    if ( !params.stops && !params.routes ) {
      create_error_with( "No 'routes' or 'stops' provided", callback );
      return;
    } else {
      params.direction ? request += '&dir='   + params.direction : undefined;
      params.routes    ? request += '&rt='    + params.routes    : undefined;
      params.stops     ? request += '&stpid=' + params.stops     : undefined;
    }

    $.ajax({
      url     : request,
      success : function( response ) {
        if ( !handle_error_with( response, callback ) ) {
          var bulletins = jsonify( response, 'sb', {
            'nm'   : 'description',
            'sbj'  : 'subject',
            'dtl'  : 'detail',
            'brf'  : 'brief',
            'prty' : 'priority',
            'srvc' : 'service'
          });
          callback.call( this, bulletins );
        }
      }
    });
  };
}