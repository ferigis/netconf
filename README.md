
# Netconf

__Authors:__ Felipe Ripoll. ([`ferigis@gmail.com`](mailto:ferigis@gmail.com)).

This project is an implementation of the Netconf specification [RFC6241](https://tools.ietf.org/html/rfc6241) written in Erlang

That code was inspired in [enetconf](https://github.com/FlowForwarding/enetconf) library.

**Note** This project is still under development.

## Before start

This projects run over ssh connection. Actually this server is an ssh subsystem. For this reason we have to think in the authentication we want to use.

So far, our netconf has two authentication ways:

>- Public key auth
>- User/Password auth.

If the first one fails, then the user/password auth is tried.

In order to config the Public Key Authentication you can read [this](http://erlang.org/doc/apps/ssh/using_ssh.html#id61452). The ssh folder used by this server is on `priv/ssh`, there you should put the `authorized_keys`, `ssh_host_rsa_key` and `ssh_host_rsa_key.pub` files.
If you want to use the user/password mechanism you have to provide a list of tuples `{user, password}` in the configuration file, with the name `ssh_credentials`. ie:

>[{netconf, [
>  {host, "127.0.0.1"}
>  , {port, 8989}
>  , {device_module, fake_device}
>  , **{ssh_credentials, [{"felipe", "password"}]}**
>]}].

## Device Callback

The `netconf_device` behaviour must be implemented in order to add the device specific responses. For example the capabilities the device has.
You can see an example of it on the 'test/fake_device.erl' module.

## Build

    $ git clone https://github.com/ferigis/netconf.git
    $ cd netconf
    $ make

## Tests

For testing I have used the [ct_netconfc](http://erlang.org/doc/man/ct_netconfc.html) module from Common Tests

    $ make tests

## Copyright and License

Copyright (c) 2016 Felipe Ripoll Gisbert

**netconf** source code is licensed under the [MIT License](LICENSE.md).
