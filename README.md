# Pragmatic Authentication Library: Facebook OAuth2 workflows

collection of Facebook OAuth2 workflows for PAL

### 1. Facebook Login (OAuth2 Authorization Code Grant) workflow

For details, read the Facebook [documentation][facebook-authcode].

#### Options

You can configure several options, which you pass into `pal:new/2` or `pal:init/1` functions via map:

- `client_id` (required) -
		The ID of your app, found in your [App dashboard][facebook-app-dashboard].
- `client_secret` (required) -
		Your unique app secret, shown on the [App Dashboard][facebook-app-dashboard].
		This app secret should never be included in client-side code or in binaries that could be decompiled.
		It is extremely important that it remains completely secret as
		it is the core of the security of your app and all the people using it.
- `redirect_uri` (required) -
		This argument is required and must be the same as the original request_uri
		that you used when starting the OAuth login process.
- `scope` (optional) -
		A list of [permissions][facebook-scope] to request from the person using your app.
- `session` (recommended) -
		A session module which implements the `pt_session` behavior. For instanse, `pt_cowboy_session`.
		Used for holding a state between the request and callback.
- `request_options` (optional) -
		Request options for [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
		All by default, `[credentials, info, extra, rules]`.

#### Auth Map

Here's an example of an authentication map available in the HTTP handler
after success execution of `pal:authenticate/2` function.

```erlang
#{credentials =>
  #{access_token => <<"token-value">>,
    expires_in   => 3600}}
```

#### How to use

```erlang
W = pal:new([pal_facebook_oauth2_authcode], Options),
pal:authenticate(W, Req).
```

For more details, see [pal-example][pal-example] project.

### 2. Facebook User (Obtaining user profile information) workflow

#### Options

You can configure several options, which you pass into `pal:new/2` or `pal:init/1` functions via map:

- `request_options` (optional) -
		Request options for [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
		All by default, `[credentials, info, extra, rules]`.

#### Input

- `access_token` (required) -
		OAuth2 Access Token obtained by `pal_facebook_oauth2_authcode` workflow.

```erlang
#{credentials =>
    #{access_token => <<"token">>}}
```

#### Auth Map

Here's an example of an authentication map available in the HTTP handler
after success execution of `pal:authenticate/2` function.

```erlang
#{credentials =>
  #{access_token => <<"token-value">>,
    expires_in   => 5100317},
  info =>
  #{first_name => <<"John">>,
    last_name  => <<"Doe">>,
    name       => <<"John Doe">>,
    urls =>
    #{<<"facebook">> => <<"https://www.facebook.com/app_scoped_user_id/1234567890/">>}},
  uid => <<"1234567890">>}
```

#### How to use

```erlang
W = pal:new([[pal_facebook_oauth2_authcode, pal_facebook_oauth2_user], Options),
pal:authenticate(W, Req).
```

For more details, see [pal-example][pal-example] project.

### Documentation

See [pal][pal] and [pt-cowboy-session][pt-cowboy-session] projects for more information.

### License

Provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[facebook-authcode]:https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow/v2.0
[facebook-app-dashboard]:https://developers.facebook.com/apps
[facebook-scope]:https://developers.facebook.com/docs/facebook-login/permissions/v2.0
[hackney]:https://github.com/benoitc/hackney
[pt-cowboy-session]:https://github.com/manifest/pt-cowboy-session
[pal]:https://github.com/manifest/pal
[pal-example]:https://github.com/manifest/pal-example

