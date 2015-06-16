# Pragmatic Authentication Library: Facebook workflows

Collection of Facebook workflows for [PAL][pal].

### 1. Facebook Login (OAuth2 Authorization Code Grant) workflow

For details, read the Facebook [documentation][facebook-authcode].

#### Options

You can configure the workflow, passing the options into `pal:new/2` or `pal:group/2` functions:

- `client_id` (required) -
		The client ID were obtained from the [App dashboard][facebook-app-dashboard].
- `client_secret` (required) -
		The client secret were obtained from the  [App Dashboard][facebook-app-dashboard].
- `redirect_uri` (required) -
		The callback endpoint.
- `scope` (optional) -
		A list of [permissions][facebook-scope] to request from the person using your app.
- `request_options` (optional) -
		Request options, in the format of [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
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

If an execution of the `pal:authenticate/{2,3}` function were successful,
the authentication schema would be returned:

```erlang
#{access_token => <<"...">>,
  expires_in => <<"5183998">>}
```

See the complete example using PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### 2. Facebook User (user's profile data) workflow

#### Options

You can configure the workflow, passing the options into `pal:new/2` or `pal:group/2` functions:

- `request_options` (optional) -
		Request options, in the format of [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `access_token` -
		OAuth2 Access Token was obtained with the `pal_facebook_oauth2_authcode` workflow.

#### Authentication Schema

If an execution of the `pal:authenticate/{2,3}` function were successful,
the authentication schema would be returned:

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

See the complete example using PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[cowboy]:https://github.com/extend/cowboy
[facebook-authcode]:https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow
[facebook-app-dashboard]:https://developers.facebook.com/apps
[facebook-scope]:https://developers.facebook.com/docs/facebook-login/permissions
[hackney]:https://github.com/benoitc/hackney
[pal]:https://github.com/manifest/pal
[pal-example]:https://github.com/manifest/pal-example

