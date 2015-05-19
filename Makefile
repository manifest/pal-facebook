PROJECT = pal_facebook_oauth2

DEPS = pal_oauth2
dep_pal_oauth2 = git git://github.com/manifest/pal-oauth2.git v0.2.2

PLT_APPS = pt pal cowlib jsx hackney
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)'

include erlang.mk
