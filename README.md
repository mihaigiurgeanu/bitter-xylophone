# bitter-xylophone v2 #

Nice and quick catalog of things (books, cds, files).

Thius is the version 2. Version 2 keeps its data in a local database
file and generates the user interface at the runtime inside the javascript
code loaded on the user's web browser. It is targeted towards cataloguing
ligrary content.

## Components ##

There are 2 componets:

-   _bitter-xylophone-app_ - a REST server written in Haskell;
-   _bitter-sylofphone-js_ a clojurescript client app impementing
    the user interface.

When launching the bitter-xylophone-app, it automatically launches a
web browser displaying the user interface.

## Prerequisites ##

-   GHC 8.10.7: use [ghcup instructions](https://www.haskell.org/ghcup/)
    to setup Haskell on your system.
-   Java 11 or newer.
-   [Leiningen](https://leiningen.org).

## bitter-xylophone-app ##

Having a ghc installed on your system you simply run `cabal build` in the
source root directory.

    # cabal build

## bitter-xylophone-js ##

Building user interfaces it is automatically done by running in the source
root directory:

    # lein cljsbuild

This will copy the compiled js into the `resources/js` folder.

## Runing in dev mode ##

    # cabal run

## Putting it all together ##

To run the application, the following structure should be created:

    /app-root-folder
    /app-root-folder/bitter-xylophone-app
    /app-root-folder/bitter-xylophone-app.exe
    /app-root-folder/resources/index.html
    /app-root-folder/resources/js/
    /app-root-folder/resources/css/
    /app-root-folder/resources/font-awsome/
    /app-root-folder/resources/font-aswsome-4.5.0/
    /app-root-folder/resources/fonts/
    /app-root-folder/data.yaml

-   The `data.yaml` is the catalog database.
-   The `resources/js/` folder contains the UI code.
-   The rest of `resources` folder contains static application resources.
-   The `public` folder contains images and files referred in the `data.yaml`
    database.

TBD - packaging the application.


## Contribution guidelines ##

* Writing tests
* Code review
* Other guidelines

## Who do I talk to? ##

* Repo owner or admin
* Other community or team contact
