# SQ2_CommonServiceInterface

# ALPHA VERSION UNDER CONSTRUCTION.

## Introduction
In an Erlang application, usually we have a number of services communicating themselves to fulfill the incoming requests from the clients. These services are implemented by writing gen_server code most of the time. Some of them may be processor intensive, others could be I/O intensive. When we face to a situation where our system slows down, it is hard to find the root cause of the performance degradation. We may have some clue, which service (or gen_server) takes the resources, causing the bottleneck, on the other hand if we are able to measure basic metrics for all the services, the quest for the root cause turns to be much easier.

Implementing the metrics in each and every service takes time and have the risk of non-conformity as different developers may have different ways and ideas on how to implement a common metric.

If we had a generalized gen_server that all the services comply with, implementing common behaviors for all services would mean to code only once and use it as many times as the number of services we would like to have it implemented.

So instead of writing a gen_server for a service, we may call the generalized gen_server, to handle the request by making a call into our callback module that implements the functionalities.

## Service as a service
Using the generalized gen_server, the service itself can focus on the business logic it needs to implement. The service is wrapped around with the common code that handles the requests and provides the return value back to the requester.

This generalized gen_server is also a service. It's functionality is to handle the requests, make them concurrently processed if it is needed, and also take measurements and common service behavior. In addition, It shall keep itself open for future improvements. 

## The Common Service Interface (CSI)
CSI maintains two interfaces:

1.  The way to handle incoming requests.
2.  The behavior of the callback module where the requests are processed.

### Handling incoming requests
The call flow for any incoming request hitting the service is the following:

The caller executes a service:function(Parameters) call in order to have a result from the service. In the service.erl file - which is the API for the service - all function have the same format as to call the csi:calltype(Parameters) API of the CSI server. There are a couple of call types available that are described below.

For a given service, this is all what is needed to set up the API.

### Processing the request in the callback module

As the CSI server is the gen_server for the service, it handles the call made in the service.erl API in it's handle_call function, by calling the callback module's function(Parameters). The callback module shall return the Results to the CSI gen_server that will forward it back to the original requester.

## Real World example.
In order to quickly understand the usage of CSI, we will implement a small service, called em_service that provides three functionalities:

1. process_foo(Atom). - Returns the atom hello_world.
2. process_too_long(Atom). - Sleeps for a long time to test the timeout kill functionality
3. process_crashing(Atom). - Throws an exception.

As you will see, there are two main parts of our em_service.erl callback module:

### 1. Beavioural functions
Implementing a service needs some housekeeping. When the service server is launched through a start() or start_link(), the init_service function is called. Here, the service shall initialize it's state and return with an {ok,ServiceState} tuple. This ServiceState is used to pass the service state when starting to process each and every request.

terminate_service is the counterpart of init_service. It is called when the service is being terminated, so this is the last place where the cleanups shall be executed before a service shuts down.

When a request reaches the server, it calls the callback module's init function to initialize the processing thread. It shall return with a state that is used during the processing of a specific request. Here a DB connection could be taken from a pool for example and pass it back.

When init returned with {ok,RequestProcessingState} our gen_server calls the function that will actually process the request. These are the functions that can be found in the Second part of the callback module, and called as Service Functions.

When the request is processed, our gen_server finally calls terminate(Reason,RequestProcessingState) callback function to have a cleanup after processing a single request.

### 2. Service functions
The implementation for the service functions goes to em_service.erl:
```erlang
-module(em_service).
-behaviour(csi_server).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
%% General state of the service
-record(em_state,{}).

%% Lifecycle State for every requests'
-record(em_session_state{}).

-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2]).

init_service(_InitArgs) ->
    {ok,#em_state{}}.

init(_Args,ServiceState) ->
    {ok,#em_session_state{}}.

terminate(Reason,_State) ->
    ok.
    
terminate_service(_Reason,_State) ->
    ok.

%% ====================================================================
%% Service functions
%% ====================================================================
-export([process_foo/2,
         process_too_long/2,
         process_crashing/2]).

process_foo(_Args,State) ->
    {hello_world,State}.

process_too_long(_Args,State) ->
    timer:sleep(100000),
    {long_job_fininshed,State}.

process_crashing(Args,State) ->
    A = Args - Args,
    {A,State}.
```

So far we have the business logic implemented. Let us see, how to create the API for the service

### Service API
The entry points to a service is very similar to a gen_server implementation. There are a couple of housekeeping functions like start, start_link, stop and then the exposed functionality is declared.

When launching a service, it's start or start_link function shall be called. As our em service uses the CSI gen server, it needs to tell the CSI server, on what name the service shall be registered locally and also, what is the module that implements the callback functions along with the functionality for the service. So start and start_link have two parameters, the name of the service and the service module. In our case the latter is em_service.erl as created above.

The business logic is called through the CSI server, by csi:call_p(?SERVICE_NAME,function,[Arg1,Arg2,..]). What happens then, is the CSI server code calls your service module function to perform the operation. You might recognized, we used call_p instead of the simple call(). There are several ways a server can process a request. It can do parallel or serialized request processing. When we use call_p() the CSI server spawns a separate process for handling the request. In this process, the callback module's init(), function() and terminate() will be called as described above. call_p() is to be used for concurrent request processing. call_s() is similar, but  no spawned process will handle the request. It means, the requests are serialized so handled one by one. Both ways, the requestor is blocked as long as the callback function returns.

There is serveral ways to call a service, here I would mention just one additionally. If instead of call_p() we use post_p() it will immediately return with a tuple containing the Pid and a reference of the process spawned for the request and later the result will be sent back to the requestor in a message.

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

process_foo(Atom) -> csi:call_p(?SERVICE_NAME,process_foo,[From]).
process_too_long(Atom) -> csi:call_p(?SERVICE_NAME,process_too_long,[From]).
process_crashing(Atom) -> csi:call_p(?SERVICE_NAME,process_crashing,[From]).
```
#To Be Continued.....
