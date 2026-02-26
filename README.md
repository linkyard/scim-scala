# scim-scala
Support classes to implement an SCIM 2 server in Scala. Implements the REST Interface, you need to provide the interface to the actual user directory.

## Usage

Import in your build.sbt

`libraryDependencies += "ch.linkyard.scim" %% "scim-scala-core" % "<version>"`

Then implement the SPI (see package `scim.spi`) and expose the rest interface in `scim.rest`.

You need to implement:

- UserStore: CRUD and search users
- GroupStore: CRUD and search groups and group memberships
- A glue layer for the REST resources (user, group, schemas, resourceType, serviceProviderConfig)

## Publish to maven central

- Make sure your GPG key is loaded (eg using `gpg --detach-sign --armor --use-agent --output - README.md`)
- `project core`
- see if everything is ok by doing `clean` and then `test`
- update version to non-SNAPSHOT in `version.sbt`
- use `publishSigned` to build
- `sonaUpload`.
- Visit <https://central.sonatype.com/publishing> to check an publish it
- Push the changes
- Create a release in github <https://github.com/linkyard/scim-scala/releases/new>
  * Name: The version, e.g. `1.0.0`
  * Release Notes: not all things that changed
