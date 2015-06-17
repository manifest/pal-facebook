# Pragmatic Authentication Library: Facebook workflows

Collection of Facebook workflows for [PAL][pal].

### 1. Facebook Login (OAuth2 Authorization Code Grant) workflow

For details, read the Facebook [documentation][facebook-oauth2-authcode].

#### Options

You can configure the workflow by passing options below into `pal:new/2` or `pal:group/2` functions:

- `client_id` (required) -
		The client ID obtained from the [App dashboard][facebook-app-dashboard].
- `client_secret` (required) -
		The client secret obtained from the [App Dashboard][facebook-app-dashboard].
- `redirect_uri` (required) -
		The client redirection endpoint.
		After completing its interaction with the resource owner,
		the authorization server directs the resource owner's user-agent to this uri.
- `scope` (optional) -
		A list of requested [permissions][facebook-oauth2-scope].
- `request_options` (optional) -
		Options for the [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication schema to be processed by the workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `code` -
		The authorization code.
- `state` -
		The state was previously passed to the authentication provider.
- `error`
		If the request fails due to a missing, invalid, or mismatching
		redirection URI, or if the client identifier is missing or invalid.

#### Authentication Schema

An successful execution of `pal:authenticate/{2,3}` function returns
the authentication schema below.

```erlang
#{access_token => <<"...">>,
  expires_in => <<"5183998">>}
```

See a complete example with PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### 2. Facebook User (user's profile data) workflow

#### Options

You can configure the workflow by passing options below into `pal:new/2` or `pal:group/2` functions:

- `request_options` (optional) -
		Options for the [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication schema to be processed by the workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `access_token` -
		An access Token obtained using the `pal_facebook_oauth2_authcode` workflow.

#### Authentication Schema

An successful execution of `pal:authenticate/{2,3}` function returns
the authentication schema below.

```erlang
#{uid => <<"...">>,
  info =>
    #{name => <<"John Doe">>,
      first_name => <<"John">>,
      last_name => <<"Doe">>,
      gender => <<"male">>,
      email => <<"john@example.com">>,
      image => <<"https://graph.facebook.com/...">>,
      uri => <<"https://www.facebook.com/...">>}}
```

See a complete example with PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[cowboy]:https://github.com/extend/cowboy
[facebook-oauth2-authcode]:https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow
[facebook-oauth2-scope]:https://developers.facebook.com/docs/facebook-login/permissions
[facebook-app-dashboard]:https://developers.facebook.com/apps
[hackney]:https://github.com/benoitc/hackney
[pal]:https://github.com/manifest/pal
[pal-example]:https://github.com/manifest/pal-example

