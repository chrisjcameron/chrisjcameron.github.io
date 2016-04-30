---
layout: post
title: Python Anaconda, Homebrew and iGraph
---

### Using Python Anaconda with iGraph
Anaconda all-included distribution is a great way to simplify the setup and management of Python installations for new users. Occasionally, the libraries bundled with Anaconda take priority over the default or Homebrew versions and cause issues when building extensions. I ran into this issue while trying to install python-igraph on a clean install of OS X 10.11, with [homebrew](brew.sh) installed igraph C library and [Anaconda](https://anaconda.org/) Python 2.7.

#### The symptoms

Upon `pip install python-igraph` I was getting errors:

     grep: /usr/lib/libiconv.la: No such file or directory
     sed: /usr/lib/libiconv.la: No such file or directory
     libtool: link: `/usr/lib/libiconv.la' is not a valid libtool archive

After forcing Anaconda from the path, I was able to get a successful build but could not `import igraph` in the python console:

```
ImportError: dlopen(/Users/cjc73/anaconda/lib/python2.7/site-packages/igraph/_igraph.so, 2): Library not loaded: libxml2.2.dylib
Referenced from: /Users/cjc73/anaconda/lib/python2.7/site-packages/igraph/_igraph.so
Reason: Incompatible library version: _igraph.so requires version 12.0.0 or later, but libxml2.2.dylib provides version 10.0.0 
```

#### A solution

I read suggestions to uninstall various components, rename library files and other solutions that do not pass the system admin smell test. I wanted a solution that left the installations alone and used the components I specified. These steps resulted in a working python-igraph installation that passed the `igraph.test` test suite. 


`1.` Temporarily change your PATH to avoid anaconda directories.

    % PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin

`2.` Try this first if you think brew igraph install is messed up:

    % brew unlink igraph
    % brew uninstall graph
    % brew update
    % brew doctor

`3.` Install homebrew iGraph normally: 

    % brew install igraph
    
   or if you don't have homebrew/science/ tapped,
   
    % brew install homebrew/science/igraph

`4.` Use extra flags so python-igraph is not built against the libraries bundled with Anaconda:

    ~/anaconda/bin/pip install python-igraph --global-option=build_ext --global-option="-L/usr/lib:/usr/local/lib"
   