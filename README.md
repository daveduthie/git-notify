
# Table of Contents

1.  [Git-Notify](#org8490ece)
    1.  [Usage](#org4135593)
    2.  [Installation](#org9ffd2c8)
    3.  [Author](#org74085a5)
    4.  [Copyright](#org1773dc3)


<a id="org8490ece"></a>

# Git-Notify


<a id="org4135593"></a>

## Usage

`git-notify ~/my/code/root`

[![asciicast](https://asciinema.org/a/175481.png)](https://asciinema.org/a/175481)


<a id="org9ffd2c8"></a>

## Installation

You'll need Roswell. Clone the repo somewhere Quickload can find it (for me that means symlinking my common lisp dev directory into `~/.roswell/local-projects`. You'll then be able to do `ros build git-notify.ros` which will produce a binary you can put anywhere. (The binary doesn't run any faster unfortunately, it's just dependency-free.)

Then add roswell's bin path to your path:

```
# .zshrc or thereabouts
export PATH=$PATH:~/.roswell/bin
```


<a id="org74085a5"></a>

## Author

-   David Duthie


<a id="org1773dc3"></a>

## Copyright

Copyright (c) 2018 David Duthie

