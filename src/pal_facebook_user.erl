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

-module(pal_facebook_user).
-behaviour(pal_authentication).
-behaviour(pal_workflow).

%% Workflow callbacks
-export([
	decl/0
]).

%% Authentication callbacks
-export([
	authenticate/4,
	uid/1,
	info/2
]).

%% Definitions
-define(FACEBOOK_GRAPH_API_URI, <<"https://graph.facebook.com/v2.3">>).

-define(ACCESS_TOKEN, <<"access_token">>).
-define(ID, <<"id">>).
-define(NAME, <<"name">>).
-define(FIRST_NAME, <<"first_name">>).
-define(LAST_NAME, <<"last_name">>).
-define(LINK, <<"link">>).
-define(EMAIL, <<"email">>).
-define(GENDER, <<"gender">>).

%% Types
-type data() :: #{access_token => binary()}.

%% ============================================================================
%% Workflow callbacks
%% ============================================================================

-spec decl() -> pal_workflow:declaration().
decl() ->
	Opts =
		#{request_options => [{follow_redirect, true}]},

	{pal_authentication, ?MODULE, Opts}.

%% ============================================================================
%% Authentication callbacks
%% ============================================================================

-spec authenticate(list(module()), data(), map(), map()) -> pal_authentication:result().
authenticate(Hs, #{access_token := Token} = Data, Meta, State) ->
	#{request_options := ReqOpts} = State,

	Uri =
		<<?FACEBOOK_GRAPH_API_URI/binary, "/me",
				$?, ?ACCESS_TOKEN/binary, $=, Token/binary>>,

	case hackney:get(Uri, [], <<>>, ReqOpts) of
		{ok, 200, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{ok, jsx:decode(Body)};
		{ok, _, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{error, {facebook_graph, Body}};
		{error, Reason} ->
			exit({Reason, {?MODULE, authenticate, [Hs, Data, Meta, State]}})
	end.

-spec uid(pal_authentication:rawdata()) -> binary().
uid(Data) ->
	pt_kvlist:get(?ID, Data).

-spec info(pal_authentication:rawdata(), map()) -> map().
info([{?ID, ID}|T], M)          -> info(T, M#{image => image(ID)});
info([{?NAME, Val}|T], M)       -> info(T, M#{name => Val});
info([{?FIRST_NAME, Val}|T], M) -> info(T, M#{first_name => Val});
info([{?LAST_NAME, Val}|T], M)  -> info(T, M#{last_name => Val});
info([{?GENDER, Val}|T], M)     -> info(T, M#{gender => Val});
info([{?EMAIL, Val}|T], M)      -> info(T, M#{email => Val});
info([{?LINK, Val}|T], M)       -> info(T, M#{uri => Val});
info([_|T], M)                  -> info(T, M);
info([], M)                     -> M.

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec image(binary()) -> binary().
image(ID) ->
	<<?FACEBOOK_GRAPH_API_URI/binary, $/, ID/binary, "/picture">>.

