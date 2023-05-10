xmppgpt
=====

An XMPP bot using the ChatGPT API.

![Pidgin Session](./pidginsession.png)


Build
-----

    $ ./rebar3 compile

Run
---

Assuming you have an ejabberd server running locally with the `localhost` domain and a user called `chatgpt@localhost` using the password `password` you can just execute it from rebar:

    $ OPENAPI_KEY=<your OpenAI API KEY here> ./rebar3 shell

If you'd like to configure a different server, have a look [here](./config/sys.config).
