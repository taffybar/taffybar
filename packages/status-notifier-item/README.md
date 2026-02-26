# status-notifier-item
![Build Status](https://github.com/taffybar/status-notifier-item/actions/workflows/build.yml/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/status-notifier-item.svg?logo=haskell&label=status-notifier-item)](https://hackage.haskell.org/package/status-notifier-item) [![Stackage LTS](http://stackage.org/package/status-notifier-item/badge/lts)](http://stackage.org/lts/package/status-notifier-item) [![Stackage Nightly](http://stackage.org/package/status-notifier-item/badge/nightly)](http://stackage.org/nightly/package/status-notifier-item)

The status-notifier-item package provides a haskell implementation of the [status-notifier-item](https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/)/[app-indicator](https://github.com/ubuntu/gnome-shell-extension-appindicator/blob/master/interfaces-xml/StatusNotifierItem.xml) protocol.

StatusNotifierHost
------------------

The host server implementation provided is partial in the sense that it is headless (provides no graphical interface), and it is supposed to be used together with UI code that handles actually displaying a tray. The [gtk-sni-tray](https://github.com/taffybar/gtk-sni-tray) provides a gtk tray widget that is implemented using the status-notifier-item library. [taffybar](https://github.com/taffybar/taffybar) Uses gtk-sni-tray to provide a tray widget that can be used with the many other widgets it offers.

StatusNotifierWatcher
---------------------

This package provides a standalone status-notifier-watch binary that can be run on its own to handle the registration of status-notifier-items. By default, it is necessary to run this binary before starting either [taffybar](https://github.com/taffybar/taffybar) or [gtk-sni-tray](https://github.com/taffybar/gtk-sni-tray), becuase the trays provided in those binaries do not handle this responsibility. If this is not done, those binaries will fail at startup with this message:

```
MethodError {methodErrorName = ErrorName "org.freedesktop.DBus.Error.ServiceUnknown", methodErrorSerial = Serial 7, methodErrorSender = Just (BusName "org.freedesktop.DBus"), methodErrorDestination = Just (BusName ":1.549"), methodErrorBody = [Variant "The name org.kde.StatusNotifierWatcher was not provided by any .service files"]}
```

A client library is also provided for the StatusNotifierWatcher protocol

StatusNotifierItem
------------------

This package provides a client library for calling into status notifier items. It also provides a sample implementation of the server side of the StatusNotifierItem protocol as well as a `status-notifier-item-static` binary that uses this implementation. This binary can be used to test host and watcher implementations, but it mostly just serves as an example of how to implement the protocol using this library, as it does not really do anything useful.
