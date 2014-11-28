%% ------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ------------------------------------------------------------------

-module(pal_facebook_oauth2_user).
-behaviour(pal_authentication).

%% Authentication callbacks
-export([
	init/1,
	authenticate/3,
	uid/1,
	info/1,
	extra/1
]).

%% Definitions
-define(ACCESS_TOKEN, <<"access_token">>).
-define(INFO_URI, <<"https://graph.facebook.com/me">>).
-define(ID, <<"id">>).
-define(NAME, <<"name">>).
-define(FIRST_NAME, <<"first_name">>).
-define(LAST_NAME, <<"last_name">>).
-define(LINK, <<"link">>).
-define(FACEBOOK, <<"facebook">>).

%% Types
-type input()    :: #{credentials => #{access_token => binary()}}.
-type workflow() :: pal_authentication:workflow().

-record(state, {
	req_opts :: list()
}).

%% ==================================================================
%% Authentication callbacks
%% ==================================================================

-spec init(pal_workflow:options()) -> pal_workflow:handler(workflow()).
init(Opts) ->
	State = #state{req_opts = pt_kvterm:get(request_options, Opts, [{follow_redirect, true}])},
	pal_authentication:init({{?MODULE, State}, Opts}).

-spec authenticate(input(), Req, workflow()) -> {pal_workflow:response(), Req} when Req :: cowboy_req:req().
authenticate(#{credentials := #{access_token := Token}}, Req, W) ->
	Uri = <<?INFO_URI/binary, $?, ?ACCESS_TOKEN/binary, $=, Token/binary>>,
	State = pal_authentication:handler_state(W),
	Resp =
		case hackney:get(Uri, [], <<>>, State#state.req_opts) of
			{ok, 200, _, Ref} ->
				pal_oauth2:from_json(Ref, fun(M) ->
					M
				end);
			{ok, _, _, Ref} ->
				pal_oauth2:from_json(Ref, fun(M) ->
					{fail, M}
				end);
			{error, Reason} ->
				Message = <<"Info request failed.">>,
				error_logger:error_report([{message, Message}, {reason, Reason}]),
				{fail, Message}
		end,

	{Resp, Req}.

-spec uid(workflow()) -> binary().
uid(W) ->
	pt_map:get(?ID, pal_authentication:raw_info(W)).

-spec info(workflow()) -> map().
info(W) ->
	RawInfo = pal_authentication:raw_info(W),
	#{name       => pt_map:find(?NAME, RawInfo),
		first_name => pt_map:find(?FIRST_NAME, RawInfo),
		last_name  => pt_map:find(?LAST_NAME, RawInfo),
		urls       => #{?FACEBOOK => pt_map:find(?LINK, RawInfo)}}.

-spec extra(workflow()) -> map().
extra(W) ->
	#{raw_info => pal_authentication:raw_info(W)}.

