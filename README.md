# CommonServiceInterface

# ALPHA VERSION USE WITH CARE

## Build environment
In case you use erlang.mk and relx the Quick start section contains the necessary setup. Otherwise just make sure the dependency on csi is included in your build environment

## Quick start on using CSI with rebar3

In your application root directory, make sure the following are inserted into your rebar.config under the deps key

    {deps, [
    {csi, {git, "git@github.com:esl/CommonServiceInterface.git", {branch, "master"}}}
    ]}.

Include csi in your relx config part of rebar.config, similar to this:

    {release, {your_server, "0.0.1"},
              [csi,
               your_server,
               runtime_tools]}.
    {extend_start_script, true}.
    
In your app.src file, include csi in the applications section.

## Quick start on using CSI with erlang.mk
In your application root directory, make sure the following are inserted into your Makefile

    PROJECT = yourapplicationname
    DEPS = csi
    
    dep_csi = git git@github.com:esl/CommonServiceInterface.git master

    include erlang.mk

    # Compile flags
    # if you want to have lager used by CSI include -Dlager
    # 
    ERLC_COMPILE_OPTS += -Dlager

    # Use the same settings for compiling releases as well as for testing
    ERLC_OPTS += $(ERLC_COMPILE_OPTS)
    TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

Include csi in your relx.config, similar to this:

    {release, {your_server, "0.0.1"},
              [csi,
               your_server,
               runtime_tools]}.
    {extend_start_script, true}.
    
In your app.src file, include csi in the applications section.

    $>make run
    
You will have csi included in your application from now on.

## Recommended configuration layout

In the csi example application you can see what is the recommended configuration layout of Erlang applications here. In the config/generate-config.sh you can see the roles of different files.

In general `sys.config` will be generated from `template.sys.config` using config vars from `config.vars`. In `config.vars` there are different section for build environments. In the lack of template file, just sys.config will be used, so templating is optional. In addition you can use `override.vars` to overwrite specific config element for any application (dependencies or the main application). An example for `override.vars`

```erlang
{dev, [
    %% To set or replace the hwm of lager
    {"lager.error_logger_hwm", 500},
    %% To remove this key from the config term
    "lager.async_threshold_window"
]}.
{prod, [
    {"lager.log_root", "/var/log/stats-feed/log"}
]}.
```
String paths are converted to atom list so only config values referenced by atoms can be manipulated.

## Managing timeout values

Calling a CSI server to process a request in parallel, needs two different timeout values to be used. Client Timeout is the time spent waiting for the server to return, Server Timeout corresponds to the request being processed.

It is important to have Client Timeout greater than the Server Timeout, since a blocking call shall not be returning before the request has been processed. Otherwise if the Client Timeout is smaller than the Server Timeout, the caller may get a timeout message before the request could be processed in a wider time frame, thus there will be unwanted messages in the client's mailbox when the processed request sends the result back.

By default, the Client Timeout is set to infinity and the Server Timeout is set to 55 seconds.

In case you need to change these values, you have three options:

1.  During initialisation.
    When a CSI server is launched, you can append a property list to set the server's parameters. [{server_timeout, Value}] passed as the last argument to start() or start link() will set the Server Timeout to be used by the launching server.
2.  At runtime.
    With csi:set_options(ServerName :: atom(), Options :: property_list()) you can set the Server Timeout at runtime.
3.  At a specific call
    Calls to CSI service server goes through the the service API, where you can append the one-time Server Timeout and also the Client Timeout values. Please visit csi.erl for details.
    
When changing the values, the timeout value parameters are in milliseconds.

## Quick Start with the example
Clone the repository, go to its directory.

    $>cd example

In case you use rebar3, issue the following command:

    $>rebar3 shell
    
In case you use erlang.mk, issue the following command:

    $>make run 

You will have an erlang shell. Here is how to play with it:

    1> em:start().
    {ok,<0.83.0>}
    2> em:
    module_info/0       module_info/1       process_crashing/1
    process_foo/1       process_too_long/1  start/0
    start_link/0        stop/0
    2> em:process_foo(test).
    hello_world
    3> em:process_too_long(test).
    {error,timeout_killed}
    4> em:process_crashing(test).
    {error,exception}
    5> 19:36:30.489 [error] Exception in service when calling em_service:process_crashing([test]). error:badarith. Stacktrace:[{em_service,process_crashing,2,[{file,"src/em_service.erl"},{line,46}]},{csi_server,process_service_request,8,[{file,"src/csi_server.erl"},{line,402}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,237}]}]
    19:36:30.490 [error] CRASH REPORT Process <0.89.0> with 0 neighbours exited with reason: exception in csi_server:process_service_request/8 line 425
    
    5> csi:stats_get_all(em_service).
    [{{response_time,process_foo},{1,106,106.0,106,106}}]
    (em_server@127.0.0.1)6> csi:services
    services/0         services_status/0
    6> csi:services_status().
    [{{registered_name,em_service},
      {csi_service_state,em_service,em_service,
                         {em_state},
                         true,36888,40985,csi_stats,
                         [all],
                         [],
                         [{response_time,[{"last_nth_to_collect",10},
                                          {"normalize_to_nth",8}]}]}},
     {{registered_name,csi_service},
      {csi_service_state,csi_service,csi_service,
                         {csi_service_state},
                         true,20500,24597,csi_stats,
                         [all],
                         [],
                         [{response_time,[{"last_nth_to_collect",10},
                                          {"normalize_to_nth",8}]}]}}]
    7> em:stop().
    ok
    8>
    
And if you use lager, here is what you find in the console.log after the sequence above:

    2016-05-02 11:31:05.432 [info] <0.6.0> Application csi started on node nonode@nohost
    2016-05-02 11:31:54.417 [info] <0.6.0> Application em started on node nonode@nohost
    2016-05-02 13:52:49.517 [info] <0.6.0> Application lager started on node nonode@nohost
    2016-05-02 13:52:49.529 [info] <0.6.0> Application csi started on node nonode@nohost
    2016-05-02 13:53:36.009 [error] <0.146.0> CRASH REPORT Process <0.146.0> with 0 neighbours exited with reason: exception in
    csi_server:process_service_request/8 line 456

# Road to use CSI

## Introduction
In an Erlang application, usually we have a number of services communicating themselves to fulfill the incoming requests from the clients. These services are implemented by writing gen_server code most of the time. Some of them may be processor intensive, others could be I/O intensive. When we face to a situation that our system slows down, it is hard to find the root cause of the performance degradation. We may have some clue, which service (or gen_server) takes the resources causing the bottleneck, on the other hand if we were able to measure basic metrics for all the services, the quest for the root cause would turn to be much easier.

Implementing the metrics in each and every service takes time and have the risk of non-conformity as different developers may have different ways and ideas on how to implement a common metric.

If we had a generalized gen_server that all the services comply with, implementing common behaviors for all services would mean to code only once and use it as many times as the number of services we would like to have it implemented.

So instead of writing a gen_server for a service, we can use a generalized gen_server, to handle the request by making calls into our callback module that implements the functionalities.

## Service as a service
Using the generalized gen_server, the service we need to implement can focus on the business logic. The service is wrapped around with the common code that handles the requests and provides the return value back to the requester.

This generalized gen_server is also a service by itself. It's functionality is to handle the requests, make them concurrently processed if it is needed, and also take measurements and common service behaviours. In addition, It shall keep itself open for future improvements. 

## The Common Service Interface (CSI)
In short, CSI sits between the service API and the service logic. When a request is coming by calling the service API, the API shall call CSI and tell it how the request shall be processed. Then CSI takes the order and makes the call into the service logic (a callback module), and responds to the caller with the value the service logic gives back.

So we have two interfaces:

1.  The way to handle incoming requests.
2.  The behaviour of the callback module where the requests are processed.

### Handling incoming requests
The call flow for any incoming request hitting the service is the following. The caller executes a servicename:function(Parameters) call in order to have a result from the service. In the servicename.erl file - which is the API for the service - all function have the same format as to call the csi:calltype(Parameters) API of the CSI server. There are a couple of call types available that are described below.

For a given service, this is all what is needed to set up the API.

### Processing the request in the callback module

As the CSI server is the gen_server for the implemented service, it handles the call made in the service.erl API in it's handle_call function, by calling the callback module's function(Parameters). The callback module shall return the Results to the CSI gen_server that will forward it back to the original requester. As a practice the module name shall be servicename_services.erl

## A tiny example.
In order to quickly understand the usage of CSI, we will implement a small service, called em_service that provides three functionalities:

1. process_foo(Atom). - Returns the atom hello_world.
2. process_too_long(Atom). - Sleeps for a long time to test the timeout kill functionality
3. process_crashing(Atom). - Throws an exception.

As you will see, there are two main parts of our em_service.erl callback module:

### 1. Behavioural functions
Implementing a service needs some housekeeping. When the service server is launched through a start() or start_link(), the init_service() function is called in the callback module. Here, the service shall initialize it's global state and return with an {ok,ServiceState} tuple. This ServiceState is used to pass the global state when starting to process each and every request.

terminate_service() is the counterpart of init_service(). It is called when the service is being terminated, so this is the last place where the cleanups shall be executed before a service shuts down.

When a request reaches the server, it calls the callback module's init(Args,ServiceState) function to initialize the processing thread. The Args parameter is the same as the service request was called with. It shall return with {ok,RequestState} that is used during the processing of a specific request. Here a DB connection could be taken from a pool for example and be passed back as part of the RequestState.

When init returned with {ok,RequestState} our CSI gen_server calls the function that will process the request.

When the request is processed, our gen_server finally calls terminate(Reason,RequestState) callback function to have a cleanup after processing a single request.

These are the functions that can be found in the second part of the callback module, and called as Service Functions.

### 2. Service functions

All service functions have two arguments. The parameter passed when the function was called (can be a list or tuple in case more than one parameter is needed) and the state of the request processing that was initialized during the init call.

The implementation for the service functions goes to em_service.erl:
```erlang
-module(em_service).
-behaviour(csi_server).

%% General state of the service
-record(em_state,{}).

%% Lifecycle State for every requests'
-record(em_session_state,{}).

-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2]).

-export([process_foo/2,
         process_too_long/2,
         process_crashing/2]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
init_service(_InitArgs) ->
    {ok,#em_state{}}.

init(_Args,_ServiceState) ->
    {ok,#em_session_state{}}.

terminate(_Reason,_State) ->
    ok.
    
terminate_service(_Reason,_State) ->
    ok.

%% ====================================================================
%% Service functions
%% ====================================================================
process_foo(_Args,State) ->
    {hello_world,State}.

process_too_long(_Args,State) ->
    timer:sleep(100000),
    {long_job_fininshed,State}.

process_crashing(Args,State) ->
    A = Args - Args,
    {A,State}.
```

The service functions shall return a tuple containing {Result, NewState}.
 
So far we have the business logic implemented. Let us see, how to create the API for the service

### Service API
The entry points to a service is very similar to a gen_server implementation. There are a couple of housekeeping functions like start, start_link, stop and then the exposed functionality is declared.

When launching a service, it's start or start_link function shall be called. As our em service uses the CSI gen server, it needs to tell the CSI server, on what name the service shall be registered locally and also what the module is that implements the callback functions along with the functionality for the service. So start and start_link have two parameters, the name of the service and the service module. In our case the latter is em_service.erl as created above.

The business logic is called through the CSI server, by csi:call_p(?SERVICE_NAME,function,Args). What happens then, is the CSI server code calls your service module function to perform the operation. You might have recognized, we used call_p instead of the simple call(). There are several ways a server can process a request. It can do parallel or serialized request processing. When we use call_p() the CSI server spawns a separate process for handling the request. In this process, the callback module's init(), function() and terminate() will be called as described above. call_p() is to be used for concurrent request processing. call_s() is similar, but  no spawned process will handle the request. It means, the requests are serialized so handled one by one. Both ways, the requester is blocked as long as the callback function returns.

There are a number of other ways exist to call a service, here I would mention just one additionally. If instead of call_p() we use post_p() it will immediately return with a tuple containing the Pid and a Reference of the process spawned for the request and later the result will be sent back to the requester in an Erlang message.

Here we will take a look at call_p for simplicity.
```erlang
-module(em).

-define(SERVICE_NAME,em_service).
-define(SERVICE_MODULE,em_service).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,
         start_link/0,
         stop/0]).

-export([process_foo/1,
         process_too_long/1,
         process_crashing/1]).


start() -> csi:start(?SERVICE_NAME,?SERVICE_MODULE).
start_link() -> csi:start_link(?SERVICE_NAME,?SERVICE_MODULE).

stop() -> csi:stop(?SERVICE_NAME).

process_foo(Atom) -> csi:call_p(?SERVICE_NAME,process_foo,[Atom]).
process_too_long(Atom) -> csi:call_p(?SERVICE_NAME,process_too_long,[Atom]).
process_crashing(Atom) -> csi:call_p(?SERVICE_NAME,process_crashing,[Atom]).
```

For a more complex service, you may want to look at csi.erl for the API of CSI itself, csi_service.erl to have some feeling how a more complex service is implemented or csi_server.erl where all the magic happens.

## Extending CSI functionality
It is easy to add new functionalities to services servers if they use the common CSI framework. The only place we need to implement it is the CSI itself and all services using it will have that new functionality.

# Statistic functionality
CSI implements a statistical framework. When a request is get processed, CSI calls all the statistics that was given to it through its csi:stats_include_type(service_name,stat_type) API, where stat_type is the name of the function in the given statistics module (default is csi_stats.erl). The stats module shall implement the functions for the types. For example if there is a stat_type named response_time, there shall be a corresponding function in the stats module with the following header:

    response_time(Stage, Request, FullRequest, Ref, Params, Tab, TempTab, Timestamp) ->
    
Where the arguments are the following:

- Stage. There are three possible values the Stage can have:
    1. start. The processing of a request is about to be started.
    2. stop. The processing of the request has finished
    3. clean. There was some problem and the request processing shall not be measured, so clean up if there was something set up at start and no longer needed.
    
- Request. This is the function name as an atom that will be called to process the request.

- FullRequest. A tuple, containing internal request data:
    {ProcessingStrategy,Request,Arguments}
    ProcessingStrategy can be several atoms, it tells whether the incoming request is processed via a call, post or cast and also it tells whether the processing is done in parallel or serialized.
    Request is the same as right above.
    Arguments are the parameters the callback function for processing the request will get.
    
- Ref. Unique reference of the request. Use this to identify the request in the start, stop and clean stage.

- Params. Parameters of a statistic type. It is a property list that can be modified in runtime.

- Tab. An ets table. Here we can save general statistical information that later can be retrieved by other services. The suggested practice of using this table is that the key shall be a tuple with two atoms, like {stat_type,request} and the value can be stats specific. Save the stats results here.

- TempTab. An ets table to store temporary or permanent (as long as the service is running) metric data. The statistic function shall clean this up in case it had inserted something at start stage. The key shall be in the form of {stat_type_valuename,Request} for permanent and {stat_type_temporaryvalue,Ref} for tempoprary data like response time metric. Save your data needed for calculations here.

- Timestamp. The time stamp in usecs when the request processing has finished. So you do not need to call erlang:now() for every stats to calculate values.

### Example
In case of the already implemented response_time metric, there is a corresponding function in csi_stats.erl. When it is called with Stage = start, it saves the os:timestamp() value into the ets table with the following call:

    response_time(start, _Request, _R, Ref, _Params, _Tab, TempTab, TimeStamp) ->
    ets:insert(TempTab, {{response_time_first, Ref}, TimeStamp});

Next time the function is called shall be with Stage = stop. The response_time function, reads the above key from the ets table, makes some calculations and writes the statistics back to the table:

    ets:insert(Tab, {{response_time,Request},{NrOfRequests,
                                              CumulatedResponseTime,
                                              CumulatedResponseTime/NrOfRequests,
                                              MinRT,
                                              MaxRT}})
                                              
in the last step it deletes the {response_time_first,Ref} key from the ets table.
                                              
In case it is called with Stage = clean instead, it just simply deletes the key that was inserted at start, from the ets table:

    ets:delete(Tab, {response_time_first,Ref})
    
And this goes on. There are numerous possibilities to turn statistics for a given service, function or stats_type on and off, and this can be done at runtime. Take a look at csi_server.erl for more details.

For example:

    (em_server@127.0.0.1)14> csi:stats_get_all(em_service).
    [{{response_time,process_foo},{3,243,81.0,78,85}}]
    (em_server@127.0.0.1)15>

meaning there were 3 requests for process_foo, the total time processing these requests was 243 usecs, The average time was 81 usecs, fastest request processing was 78 usecs, slowest was 85 usecs

## Generic application functionality

In the stream above we saw how to implement a service using CSI. You might have recognized, there has been fixed part of the code especially when initializing a service. All calls for start, start_link, stop are the same for any service, and usually supervising the application is the same boring process for any app.

To ease our life, there is an additional functionality of CSI. In case we use it, we can have CSI supervising our service servers by simply add the necessary information into our sys.config file. For example if we have a service called app_service, the service functions are in app_service.erl we can instruct CSI at startup to launch our server by including the following in sys.config:

    {csi,[{servers,[{app_service,app_service,[],default}]
           }
          ]}
          
The servers tag in the tuple instruct CSI at startup to run through the list of tuples containing the information to launch a service. The format is {ServiceName, ServiceModule, InitArgs, ChildSpec}. The first three speak for themselves, the ChildSpec is the specification for the supervisor, how the service shall be added as a child. See supervisor:add_child/2 in Erlang docs.

When we have this, there are three files needs to be maintained.

The app_service.erl will be the same as above, but the app.erl will be a bit simpler. This is the API for the service and now purely contains the functionality:

```erlang
-module(app).

-define(SERVICE_NAME, app_service).
-define(SERVICE_MODULE, app_service).

%% ====================================================================
%% API functions
%% ====================================================================

-export([process_foo/1,
         process_too_long/1,
         process_crashing/1]).

process_foo(Atom) -> csi:call_p(?SERVICE_NAME, process_foo, [Atom]).
process_too_long(Atom) -> csi:call_p(?SERVICE_NAME, process_too_long, [Atom]).
process_crashing(Atom) -> csi:call_p(?SERVICE_NAME, process_crashing, [Atom]).
```

The other two files, relx.config and app.src remains the same as above.

An example is set up in the generic_app directory, so in case you would like to start developing your service, you might copy the directory, make the changes for your service and start extending it with the functionality you need.

### sys.config Child Specification

In case you need other than defaul child specification, here is an example for your sys.config:

    {csi,[{servers,[{app_service,app_service,[],
                          #{id => app_service,
                          start => {csi,
                                    start_link,
                                    [app_service, app_service, []]},
                          restart => permanent,
                          shutdown => 2000,
                          type => worker,
                          modules => [app,app_service]}}]
           }]
     }

So to use a ChildSpec as a map, instead of just using the default values that are the same as in the example here.

Feel free to come up with more lightweight stats!

## Please share your thoughts, suggest improvements, find bugs and report them!
    
