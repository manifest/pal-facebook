%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014-2015 Andrei Nesterov <ae.nesterov@gmail.com>
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
%% ----------------------------------------------------------------------------

-module(pal_facebook_oauth2_authcode).
-behaviour(pal_workflow).

%% Workflow callbacks
-export([
	decl/0
]).

%% OAuth2 AuthCode Workflow callbacks
-export([
	access_token_request/1
]).

%% Authentication workflow callbacks
-export([
	credentials/2
]).

%% Definitions
-define(ACCESS_TOKEN, <<"access_token">>).
-define(EXPIRES, <<"expires">>).
-define(ERROR, <<"error">>).

%% ============================================================================
%% Workflow callbacks
%% ============================================================================

-spec decl() -> pal_workflow:declaration().
decl() ->
	Opts =
		#{authorization_uri => <<"https://www.facebook.com/dialog/oauth">>,
			access_token_uri  => <<"https://graph.facebook.com/oauth/access_token">>,
			scope => []},

	{pal_oauth2_authcode, ?MODULE, Opts}.

%% ============================================================================
%% OAuth2 AuthCode Workflow callbacks
%% ============================================================================

-spec access_token_request(map()) -> pal_authentication:result().
access_token_request(State) ->
	#{access_token_uri := Uri,
		request_fields := Fields,
		request_options := ReqOpts} = State,

	Qs = cow_qs:qs(Fields),
	case hackney:get(<<Uri/binary, $?, Qs/binary>>, [], <<>>, ReqOpts) of
		{ok, 200, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{ok, cow_qs:parse_qs(Body)};
		{ok, _, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{error, {facebook_oauth2, Body}};
		{error, Reason} ->
			exit({Reason, {?MODULE, access_token_request, [State]}})
	end.

%% ============================================================================
%% Authentication workflow callbacks
%% ============================================================================

-spec credentials(pal_authentication:rawdata(), map()) -> map().
credentials([{?ACCESS_TOKEN, Val}|T], M) -> credentials(T, M#{access_token => Val});
credentials([{?EXPIRES, Val}|T], M)      -> credentials(T, M#{expires_in => Val});
credentials([_|T], M)                    -> credentials(T, M);
credentials([], M)                       -> M.

