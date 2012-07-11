# = Name
# Chicago Transit Authority BusTracker Service API
# 
# = Description
# The ChicagoTransit module queries the CTA BusTracker Service using the
# official version 1.0 API. CTA uses the BusTime system to provide developers
# with real-time route information through BusTracker. The BusTracker Service
# allows deveopers to make HTTP requests and recieve XML responses. The CTA
# provides {official API documentation (PDF)}[http://www.transitchicago.com/assets/1/developer_center/BusTime_Developer_API_Guide.pdf],
# and you can request an access key at the {API homepage}[http://www.transitchicago.com/developers/bustracker.aspx].
# A {wiki}[http://chicagowiki.transitapi.com/Home] was also created for the API.
#
# = Authors
# Sean Clemmer
module ChicagoTransit
  require 'nokogiri'
  require 'open-uri'
  require 'cgi'

  # === Usage
  # Convert a CTA-formatted timestamp into the standard Ruby format (without seconds).
  # === Parameters
  # - cta_time: CTA-formatted timestamp (required)
  def make_ruby_time_without_seconds_from( cta_time )
    time = cta_time.split
    t =  time[1].split(/:/).reverse
    t << time[0].gsub(/^....../, '')
    t << time[0].gsub(/^..../, '').gsub(/..$/, '')
    t << time[0].gsub(/....$/, '')
    Time.mktime( t[4], t[3], t[2], t[1], t[0] )
  end

  # === Usage
  # Convert a CTA-formatted timestamp into the standard Ruby format.
  # === Parameters
  # - cta_time: CTA-formatted timestamp (required)
  def make_ruby_time_from( cta_time )
    time = cta_time.split
    t =  time[1].split(/:/).reverse
    t << time[0].gsub(/^....../, '')
    t << time[0].gsub(/^..../, '').gsub(/..$/, '')
    t << time[0].gsub(/....$/, '')
    Time.mktime( t[5], t[4], t[3], t[2], t[1], t[0] )
  end

  # === Usage
  # Convert a standard Ruby timestamp into the CTA format (without seconds).
  # === Parameters
  # - time: Standard Ruby timestamp (optional)
  def make_cta_time_without_seconds_from( time=Time.now )
    return time.strftime( "%Y%m%d %H:%M" )
  end

  # === Usage
  # Convert a standard Ruby timestamp into the CTA format.
  # === Parameters
  # - time: Standard Ruby timestamp (optional)
  def make_cta_time_from( time=Time.now )
    return time.strftime( "%Y%m%d %H:%M:%S" )
  end

  # = Tracker
  # Ruby implementation of the CTA BusTracker system.
  # Most instance methods implemented here return an array or an array of hashes when successful.
  # Response errors from the system raise a runtime error. Other errors may be resuced to nil.
  # = Example Usage
  # Afer requiring this file and including the ChicagoTransit module in +irb+:
  #  >> tracker = ChicagoTransit::Tracker.new( 'your_api_key_here' )
  #  => #<ChicagoTransit::Tracker:0x1006197d8>
  #  >> tracker.system_time
  #  => Mon Jan 18 20:06:04 -0600 2010
  #  >> tracker.routes                      
  #  => [{"number"=>"1", "description"=>"Indiana/Hyde Park"}, ...
  #  >> tracker.vehicles :routes => [ '55' ]       
  #  => [{"number"=>"1731", "route"=>"55", "latitude"=>"41.791778564453125", ...
  #  >> tracker.bulletins :routes => [ '171' ]
  #  => [{"service"=>#<Nokogiri::XML::Element:0x80dccec4 name="srvc" ...
  class Tracker
    attr_reader :api_key

    # === Usage
    # Construct a new instance of the Tracker.
    # === Parameters
    # - API key: 25-digit CTA BusTracker API access key (required)
    def initialize( api_key )
      @@api_key   = api_key
      @@api_url   = 'http://www.ctabustracker.com/bustime/api/v1/'
      @@key_query = '?key=' + @@api_key
    end

    # === Usage
    # Retrieve the system date and (local) time.
    # Uses ChicagoTransit::make_ruby_time_from( cta_time ) to convert the CTA format to Ruby standards.
    # === Base URL
    # http://www.ctabustracker.com/bustime/api/v1/gettime
    def system_time
      request  = @@api_url + 'gettime' + @@key_query
      response = Nokogiri::XML( open( request ) )
      year = '', month = '', day = '', hour = '', min = '', sec = ''
      response.xpath('//bustime-response/error').length.times do |index|
        raise 'CTA: ' + response.xpath("//bustime-response/error[#{index+1}]/msg").first.content
      end
      return ChicagoTransit::make_ruby_time_from response.xpath( '//bustime-response/tm' ).first.content rescue nil
    end

    # === Usage
    # Retrieve information for vehicles serviced by the specified list or routes.
    # === Base URL
    # http://www.ctabustracker.com/bustime/api/v1/getvehicles
    # === Parameters
    # - routes: List of CTA route designators (not available with vehicles parameter)
    # - vehicles: List of CTA vehicle IDs (not available with routes parameter)
    # At least one required. If both routes and vehicles are provided, routes are used.
    def vehicles( params )
      request    = @@api_url + 'getvehicles' + @@key_query
      params[:routes].map! { |i| CGI::escape i } rescue nil
      params[:vehicles].map! { |i| CGI::escape i } rescue nil
      routes     = params[:routes].join( ',' ) rescue ''
      vehicles   = params[:vehicles].join( ',' ) rescue ''
      response   = Nokogiri::XML( open( request + '&rt=' + routes + '&vidd=' + vehicles ) )
      vehicles   = Array.new
      properties = {
        'number'      => 'vid',
        'timestamp'   => 'tmstmp',
        'latitude'    => 'lat',
        'longitude'   => 'lon',
        'heading'     => 'hdg',
        'pattern'     => 'pid',
        'distance'    => 'pdist',
        'route'       => 'rt',
        'destination' => 'des',
        'delayed'     => 'dly'
      }
      response.xpath('//bustime-response/error').length.times do |index|
        raise 'CTA: ' + response.xpath("//bustime-response/error[#{index+1}]/msg").first.content
      end
      response.xpath('//bustime-response/vehicle/vid').length.times do |index|
        vehicle = Hash.new
        properties.each do |name, rep|
          unless name == 'timestamp'
            vehicle["#{name}"] = response.xpath("//bustime-response/vehicle[#{index+1}]/#{rep}").first.content rescue nil
          else
            vehicle["#{name}"] = ChicagoTransit::make_ruby_time_without_seconds_from response.xpath("//bustime-response/vehicle[#{index+1}]/#{rep}").first.content rescue nil
          end
        end
        vehicles << vehicle
      end
      return vehicles
    end

    # === Usage
    # Retrieve a list of routes serviced by the system.
    # === Base Url
    # http://www.ctabustracker.com/bustime/api/v1/getroutes
    def routes
      request    = @@api_url + 'getroutes' + @@key_query
      response   = Nokogiri::XML( open( request ) )
      routes     = Array.new
      properties = {
        :number      => 'rt',
        :description => 'rtnm'
      }
      response.xpath('//bustime-response/error').length.times do |index|
        raise 'CTA: ' + response.xpath("//bustime-response/error[#{index+1}]/msg").first.content
      end
      response.xpath('//bustime-response/route/rt').length.times do |index|
        route = Hash.new
        properties.each do |name, rep|
          route["#{name}"] = response.xpath("//bustime-response/route[#{index+1}]/#{rep}").first.content rescue nil
        end
        routes << route
      end
      return routes
    end

    # === Usage
    # Retrieve a set of directions serviced by the specified route.
    # === Base Url
    # http://www.ctabustracker.com/bustime/api/v1/getdirections
    # === Parameters
    # - route: Single route designator (required)
    def directions( params )
      request    = @@api_url + 'getdirections' + @@key_query
      route      = CGI::escape params[:route].to_s
      response   = Nokogiri::XML( open( request + '&rt=' + route ) )
      directions = Array.new
      properties = {
        'direction' => 'dir'
      }
      response.xpath('//bustime-response/error').length.times do |index|
        raise 'CTA: ' + response.xpath("//bustime-response/error[#{index+1}]/msg").first.content
      end
      response.xpath('//bustime-response/dir').length.times do |index|
        direction = Hash.new
        properties.each do |name, rep|
          direction["#{name}"] = response.xpath("//bustime-response/#{rep}[#{index+1}]").first.content rescue nil
        end
        directions << direction
      end
      return directions
    end

    # === Usage
    # Retrieve a set of stoped serviced by the specified route and direction.
    # === Base Url
    # http://www.ctabustracker.com/bustime/api/v1/getstops
    # === Parameters
    # - route: Single route designator (required)
    # - direction: Sing route direction (required)
    def stops( params )
      request    = @@api_url + 'getstops' + @@key_query
      route      = CGI::escape params[:route].to_s
      direction  = CGI::escape params[:direction].to_s
      response   = Nokogiri::XML( open( request + '&rt=' + route + '&dir=' + direction ) )
      stops      = Array.new
      properties = {
        'number'      => 'stpid',
        'description' => 'stpnm',
        'latitude'    => 'lat',
        'longitude'   => 'lon'
      }
      response.xpath('//bustime-response/error').length.times do |index|
        raise 'CTA: ' + response.xpath("//bustime-response/error[#{index+1}]/msg").first.content
      end
      response.xpath('//bustime-response/stop').length.times do |index|
        stop = Hash.new
        properties.each do |name, rep|
          stop["#{name}"] = response.xpath("//bustime-response/stop[#{index+1}]/#{rep}").first.content rescue nil
        end
        stops << stop
      end
      return stops
    end

    # === Usage
    # Retrieve a set of patterns serviced by the specified list or routes.
    # Patterns are composed of geo-positional points that may be used to construct a map of the pattern (i.e. route variation).
    # === Base Url
    # http://www.ctabustracker.com/bustime/api/v1/getpatterns
    # === Parameters
    # - route: Single route designator (not available with patterns parameter)
    # - patterns: List of pattern IDs (not available with route parameter)
    # At least one required. If both route and patterns are provided, route is used.
    def patterns( params )
      # Todo: Encode points into their own hash  
      request    = @@api_url + 'getpatterns' + @@key_query
      params[:patterns].map! { |i| CGI::escape i } rescue nil
      route      = CGI::escape params[:route].to_s
      numbers    = params[:patterns].join(',') rescue ''
      response   = Nokogiri::XML( open( request + '&rt=' + route + '&pid=' + numbers ) )
      patterns   = Array.new
      properties = {
        'number'      => 'pid',
        'length'      => 'ln',
        'direction'   => 'rtdir',
        'points'      => 'pt'
      }
      response.xpath('//bustime-response/error').length.times do |index|
        raise 'CTA: ' + response.xpath("//bustime-response/error[#{index+1}]/msg").first.content
      end
      response.xpath('//bustime-response/ptr').length.times do |index|
        pattern = Hash.new
        properties.each do |name, rep|
          unless name == 'points'
            pattern["#{name}"] = response.xpath("//bustime-response/ptr[#{index+1}]/#{rep}").first.content rescue nil
          else
            pattern["#{name}"] = response.xpath("//bustime-response/ptr[#{index+1}]/#{rep}").first rescue nil
          end
        end
        patterns << pattern
      end
      return patterns
    end

    # === Usage
    # Retrieve a set of predictions for the specified stops or vehicles.
    # === Base Url
    # http://www.ctabustracker.com/bustime/api/v1/getpredictions
    # === Parameters
    # - stops: List of stop IDs (not available with vehicles parameter)
    # - routes: List of route designators (optional, only available with stops parameter)
    # - vehicles: List of vehicle IDs (not available with stops parameter)
    # - ceiling: Maximum number of predictions to retrieve (optional)
    # At least stops or vehicles required. If both stops and vehiles are provided, stops are used.
    def predictions( params )
      request     = @@api_url + 'getpredictions' + @@key_query
      params[:stops].map! { |i| CGI::escape i } rescue nil
      params[:routes].map! { |i| CGI::escape i } rescue nil
      params[:vehicles].map! { |i| CGI::escape i } rescue nil
      stops       = params[:stops].join(',') rescue ''
      routes      = params[:routes].join(',') rescue ''
      vehicles    = params[:vehicles].join(',') rescue ''
      ceiling     = CGI::escape params[:ceiling].to_s
      response    = Nokogiri::XML( open( request + '&stpid=' + stops + '&rt=' + routes + '&vid=' + vehicles + '&top=' + ceiling ) )
      predictions = Array.new
      properties  = {
        'timestamp'   => 'tmstmp',
        'type'        => 'typ',
        'stop'        => 'stpid',
        'description' => 'stpnm',
        'vehicle'     => 'vid',
        'distance'    => 'dstp',
        'route'       => 'rt',
        'destination' => 'des',
        'prediction'  => 'prdtm',
        'delayed'     => 'dly'
      }
      response.xpath('//bustime-response/error').length.times do |index|
        raise 'CTA: ' + response.xpath("//bustime-response/error[#{index+1}]/msg").first.content
      end
      response.xpath('//bustime-response/prd').length.times do |index|
        prediction = Hash.new
        properties.each do |name, rep|
          unless name == 'prediction' or name == 'timestamp'
            prediction["#{name}"] = response.xpath("//bustime-response/prd[#{index+1}]/#{rep}").first.content rescue nil
          else
            prediction["#{name}"] = ChicagoTransit::make_ruby_time_without_seconds_from response.xpath("//bustime-response/prd[#{index+1}]/#{rep}").first.content rescue nil
          end
        end
        predictions << prediction
      end
      return predictions
    end

    # === Usage
    # Retrieve a set of service bulletins in effect for the specified routes or stops.
    # === Base Url
    # http://www.ctabustracker.com/bustime/api/v1/getservicebulletins
    # === Parameters
    # - routes: List of route designators (required without stops parameter)
    # - direction: Single route direction (optional)
    # - stops: List of stop IDs (required without routes parameter)
    def bulletins( params )
      # Todo: Encode services into their own hash
      request     = @@api_url + 'getservicebulletins' + @@key_query
      params[:routes].map! { |i| CGI::escape i } rescue nil
      params[:stops].map! { |i| CGI::escape i } rescue nil
      routes      = params[:routes].join(',') rescue ''
      direction   = CGI::escape params[:direction].to_s rescue ''
      stops       = params[:stops].join(',') rescue ''
      response    = Nokogiri::XML( open( request + '&stpid=' + stops + '&rt=' + routes + '&dir=' + direction ) )
      bulletins   = Array.new
      properties  = {
        'description' => 'nm',
        'subject'     => 'sbj',
        'detail'      => 'dtl',
        'brief'       => 'brf',
        'priority'    => 'prty',
        'service'     => 'srvc'
      }
      response.xpath('//bustime-response/error').length.times do |index|
        raise 'CTA: ' + response.xpath("//bustime-response/error[#{index+1}]/msg").first.content
      end
      response.xpath('//bustime-response/sb').length.times do |index|
        bulletin = Hash.new
        properties.each do |name, rep|
          unless name == 'service'
            bulletin["#{name}"] = response.xpath("//bustime-response/sb[#{index+1}]/#{rep}").first.content rescue nil
          else
            bulletin["#{name}"] = response.xpath("//bustime-response/sb[#{index+1}]/#{rep}").first rescue nil
          end
        end
        bulletins << bulletin
      end
      return bulletins
    end
  end
end