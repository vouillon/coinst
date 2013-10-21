coinst
======

Collection of tools to find issues in package repositories and help manage Debian package integration.

- `coinst` creates a graph that show all conflicts between packages while being orders of magnitude smaller than the whole graph of dependency and conflicts;
- `coinst-upgrades` finds set of packages that could be installed together in a previous version of a Debian repository and cannot be installed any longer;
- `comigrate` is a tool designed to help manage the migration of packages from Debian unstable to testing.
