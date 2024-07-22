# Mit
We're not trying to get into MIT. This project aims to recreate and extend the capabilities of the version control system, Git. Systems like Git are imperative for large scale projects where teams need to work on the same files simultaneously. Users will be able to create and maintain repositories stored on a local computer. Our system will allows users to clone and pull repos, create branches, edit project files, commit, add, push, merge, deal with merge conflicts, revert edit history. Time permitting, we would like to expand the system to store projects in an actual database, allowing for true multi-user collaboration, and we’d like to develop a user-friendly graphic interface that allows users to view repo history.

## Team Members
Nidhi Mylavarapu (nm549)
Sia Chitnis (sc2665)
Michael Ngo (mn542)
Mahitha Penmetsa (msp259)

## Architecture
```bash
.
├── bin
│   └── main.ml # the mit executable
├── cmd
│   ├── cfactory.ml # links parameters to commands
│   ├── command.ml
│   ├── params.ml 
│   └── param.ml # string and boolean parameters
├── src
│   ├── mit.ml # mit commands
│   ├── mitnames.ml # centralize naming conventions
│   ├── branch.ml 
│   ├── commit.ml
│   ├── tree.ml
│   ├── blob.ml 
│   ├── reader.ml
│   ├── writer.ml
│   ├── hash.ml
│   ├── time.ml
│   └── exception.ml
└── test
    ├── main.ml # OUnit suite
    └── test-files
```

## Getting Started
Refer to the [installation guide](INSTALL.md).

## Examples
```bash
$ mit help
```

```bash
$ mit init
$ mit clean
```

```bash
$ mit init
$ mit commit "my name" "initial commit"
$ mit branch -b "secondary"
$ mit checkout "secondary" 
```

```bash
$ mit init
$ mit status
$ mit commit Author1 "Import code"
$ mit commit Author2 "Edit code"
$ mit commit Author3 "Fix code"
$ mit change
$ mit log
```
