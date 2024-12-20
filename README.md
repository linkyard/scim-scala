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

- `project core`
- see if everything is ok by doing `clean` and then `test`
- update version to non-SNAPSHOT in version.sbt
- use `publishSigned` to build
- `sonatypeBundleRelease`.
- you can check the progress on <https://central.sonatype.com/>

Note that you need to have `~/.sbt/1.0/sonatype.sbt` with the credentials, e.g. 
```
credentials += Credentials("Maven Central",
    "central.sonatype.com",
    "token-user",
    "token-password"
)
```
